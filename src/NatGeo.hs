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
import Data.Aeson
import Data.Aeson.Lens
import System.Console.Spinners

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
type PhotoBaseJSON = "photography" :> "photo-of-the-day" :> "_jcr_content" :> ".gallery.json" :> Get '[JSON] Value
type PhotoUrl = "u" :> Capture "hashId" ImageHashId :> Get '[JPG] ByteString

newtype ImageHashId = ImageHashId Text
  deriving Show

instance ToHttpApiData ImageHashId where
  toUrlPiece (ImageHashId txt) = txt

photoBase :: ClientM Node
photoBase = client (Proxy :: Proxy PhotoBase)

photoBaseJSON :: ClientM Value
photoBaseJSON = client (Proxy :: Proxy PhotoBaseJSON)

photoUrl :: ImageHashId -> ClientM ByteString
photoUrl = client (Proxy :: Proxy PhotoUrl)

getUrl :: (Applicative f, Contravariant f, HasElement a) =>
     (Text -> f Text) -> a -> f a
getUrl = allAttributed (ix "property" . only "og:image") . Text.Taggy.Lens.attrs . at "content" . traverse

photoOfTheDayClientEnv :: IO ClientEnv
photoOfTheDayClientEnv = do
  mgr <- C.newManager tlsManagerSettings
  return $ ClientEnv mgr (BaseUrl Https "www.nationalgeographic.com" 443 "") Nothing

yourshotClient :: IO ClientEnv
yourshotClient = do
  mgr <- C.newManager tlsManagerSettings
  return $ ClientEnv mgr (BaseUrl Https "yourshot.nationalgeographic.com" 443 "") Nothing

download :: FilePath -> IO ()
download destFp = do
  env <- photoOfTheDayClientEnv
  eResponse <- spinner "Requesting Photo of the Day page." $ runClientM photoBaseJSON env
  case eResponse of
    Left err -> print err
    Right response -> case response ^? to getLargestImage . traverse of
      Nothing -> putStrLn "Could not find the url on the page!"
      Just (size, imgUrl) -> do
        putStrLn $ "Found an image of size " <> tshow size <> " at " <> imgUrl
        case imgUrl ^? getImageHashId of
          Nothing -> putStrLn "Could not get the Image Hash from the image url!"
          Just hash -> do
            yourshotEnv <- yourshotClient
            resp <- spinner "Downloading the image now!" $ runClientM (photoUrl hash) yourshotEnv
            case resp of
              Left err -> print err
              Right bs -> writeFile destFp bs

itemsF :: Fold Value Value
itemsF = key "items" . plate

sizesF :: Fold Value (Int, Text)
sizesF = key "sizes" . _Object . to mapToList . traverse . runFold ((,) <$> Fold (_1 . to readMay . traverse) <*> Fold (_2 . _String))

largestSize :: [(Int, Text)] -> Maybe (Int, Text)
largestSize = maximumByMay (\x y -> compare (fst x) (fst y))

getLargestImage :: Value -> Maybe (Int, Text)
getLargestImage v = do
  item <- v ^? itemsF
  let sizes = item ^.. sizesF
  largestSize sizes

getImageHashId :: Fold Text ImageHashId
getImageHashId = to (reverse . splitElem '/') . dropping 1 traverse . to ImageHashId
