{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module ProgressEmail.Server where

import ClassyPrelude hiding (Handler)
import Servant
import ProgressEmail.Types
import ProgressEmail.Form
import Servant.HTML.Lucid
import Lucid
import Text.Digestive
import Network.Wai.Handler.Warp

type TicketFormPost = "ticket" :> QueryParam "EPC-" Text :> QueryParam "Description" Text :> Post '[HTML] (Html ())
type TicketFormGet = "ticket" :> Get '[HTML] (Html ())

type API
  = TicketFormPost
  :<|> TicketFormGet

api :: Proxy API
api = Proxy

ticketFormHandler :: Maybe Text -> Maybe Text -> Handler (Html ())
ticketFormHandler (Just ticket) (Just desc) = do
  putStrLn "Running post handler now!"
  (v, mTicket) <- postForm "ticket" ticketForm (\_ -> pure $ postTicketForm ticket desc)
  pure $ do
    ticketHtml v
    p_ $ toHtml $ tshow mTicket
ticketFormHandler _ _ = do
  pure $ pure ()

ticketFormGetHandler :: Handler (Html ())
ticketFormGetHandler = do
  v <- getForm "ticket" ticketForm
  pure $ ticketHtml v

server :: Application
server = serve api (ticketFormHandler :<|> ticketFormGetHandler)
