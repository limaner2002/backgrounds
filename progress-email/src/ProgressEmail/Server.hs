{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module ProgressEmail.Server where

import ClassyPrelude hiding (Handler)
import Servant
import Servant.Utils.Links
import ProgressEmail.Types
import ProgressEmail.Form hiding (checkErrors)
import ProgressEmail.Tupleable
import Servant.HTML.Lucid
import Lucid
import Text.Digestive
import Network.Wai.Handler.Warp
import qualified Web.FormUrlEncoded as WF
import Data.Aeson (eitherDecode, encode)
import ProjectM36.Client.Simple (DbError, RelationalError)
import Control.Monad.Error.Class
import qualified Html as H
import qualified Data.ByteString.Lazy as BL
import ProgressEmail.Html (renderTicket, renderTicketTable)

data TYHTML

instance MimeRender TYHTML [Ticket a] where
  mimeRender _ = H.renderByteString . renderTicketTable

instance Accept TYHTML where
  contentType _ = contentType (Proxy :: Proxy HTML)

type StyleSheet = "style.css" :> Get '[OctetStream] ByteString
type TicketFormPost = "ticket" :> "new" :> ReqBody '[FormUrlEncoded] WF.Form :> Post '[HTML] (Html ())
type TicketFormGet = "ticket" :> "new" :> Get '[HTML] (Html ())
type Tickets = "ticket" :> Get '[TYHTML] ([Ticket [Int]])

type API
  = TicketFormPost
  :<|> TicketFormGet
  :<|> StyleSheet
  :<|> Tickets

api :: Proxy API
api = Proxy

ticketFormHandler :: FilePath -> WF.Form -> Handler (Html ())
ticketFormHandler path form = do
  tickets <- queryTickets path >>= checkQueryResult
  (v, mTicket) <- postForm "ticket" (ticketForm tickets) (\_ -> pure $ postTicketForm form)
  case mTicket of
    Nothing -> pure ()
    Just ticket ->
      liftIO
        .   insertTickets path
        >=> checkErrors
        >=> checkErrors
        $   [ticket]
      -- writeFile path $ toStrict $ encode $ ticket : tickets
      -- res <- liftIO $ updateTicket path ticket
      -- res <- liftIO $ insertTickets path ticket
      -- checkErrors >=> checkErrors $ res
  pure $ page $ do
    ticketHtml v
    p_ $ toHtml $ tshow mTicket

ticketFormGetHandler :: FilePath -> Handler (Html ())
ticketFormGetHandler path = do
  tickets <- queryTickets path >>= checkQueryResult
  v <- getForm "ticket" (ticketForm tickets)
  pure $ page $ ticketHtml v

getTickets :: FilePath -> Handler [Ticket [Int]]
getTickets dbPath = queryTickets dbPath >>= checkQueryResult

serveStyleSheet :: Handler ByteString
serveStyleSheet = readFile "progress-email/src/ProgressEmail/format.css"

server :: FilePath -> Application
server path = serve api (ticketFormHandler path :<|> ticketFormGetHandler path :<|> serveStyleSheet :<|> getTickets path)

styleURI :: URI
styleURI = linkURI $ safeLink (Proxy :: Proxy API) (Proxy :: Proxy StyleSheet)

page :: Html () -> Html ()
page body = do
  head_ (link_ [rel_ "stylesheet", href_ (pack $ uriPath styleURI)])
  body_ body

loadTickets :: MonadIO m => FilePath -> m (Either String [Ticket [Int]])
loadTickets path = do
  contents <- readFile path
  pure $ eitherDecode $ fromStrict contents

queryTickets :: MonadIO m => FilePath -> m (Either DbError [Either RelationalError (Ticket [Int])])
queryTickets dbPath = liftIO $ runTransaction dbPath retrieveTickets

checkQueryResult :: MonadError ServantErr m => Either DbError [Either RelationalError a] -> m [a]
checkQueryResult (Left err) = throwError $ err500 { errBody = fromStrict . encodeUtf8 $ tshow err }
checkQueryResult (Right items) = case lefts items of
  [] -> pure $ rights items
  errors -> throwError $ err500 { errBody = fromStrict . encodeUtf8 $ intercalate "\n" $ fmap tshow errors }

checkErrors :: (MonadError ServantErr m, Show err) => Either err a -> m a
checkErrors (Left err) = throwError $ err500 { errBody = fromStrict . encodeUtf8 . tshow $ err }
checkErrors (Right a) = pure a

