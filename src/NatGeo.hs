{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module NatGeo where

import ClassyPrelude
import Control.Lens
import Text.Taggy.Lens
import Servant
import Servant.Client
import Network.HTTP.Media ((//))
import qualified Network.HTTP.Client as C
import Network.HTTP.Client.TLS

data Html

instance MimeUnrender Html Node where
  mimeUnrender _ bs = case bs ^? to decodeUtf8 . html of
    Nothing -> Left "Could not decode the page!"
    Just node -> Right node

instance Accept Html where
  contentType _ = "text" // "html"

data JPG

instance Accept JPG where
  contentType _ = "image" // "jpeg"

instance MimeUnrender JPG ByteString where
  mimeUnrender _ = Right . toStrict

type PhotoBase = "photography" :> "photo-of-the-day" :> Get '[Html] Node
type PhotoUrl = "u" :> Capture "hashId" Text :> Get '[JPG] ByteString

photoBase :: ClientM Node
photoBase = client (Proxy :: Proxy PhotoBase)

photoUrl :: Text -> ClientM ByteString
photoUrl = client (Proxy :: Proxy PhotoUrl)

getUrl :: (Applicative f, Contravariant f, HasElement a) =>
     (Text -> f Text) -> a -> f a
getUrl = allAttributed (folded . only "og:image") . Text.Taggy.Lens.attrs . at "content" . traverse

photoOfTheDayClientEnv = do
  mgr <- C.newManager tlsManagerSettings
  return $ ClientEnv mgr (BaseUrl Http "nationalgeographic.com" 80 "")

yourshotClient = do
  mgr <- C.newManager tlsManagerSettings
  return $ ClientEnv mgr (BaseUrl Https "yourshot.nationalgeographic.com" 443 "")

download :: FilePath -> IO ()
download destFp = do
  env <- photoOfTheDayClientEnv
  node <- runClientM photoBase env
  let mHashId = node ^? _Right . getUrl . to (reverse . splitElem '/') . dropping 1 traverse
  case mHashId of
    Nothing -> error "Could not find the original image URL!"
    Just hashId -> do
      env <- yourshotClient
      eBS <- runClientM (photoUrl hashId) env
      case eBS of
        Left err -> print err
        Right bs -> writeFile destFp bs
