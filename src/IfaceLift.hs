{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Arrows #-}

module IfaceLift
  ( getImageUrl ) where

-- import MachineUtils
import ClassyPrelude

-- import Text.XML.HXT.Core
-- import Text.XML.HXT.HTTP
-- -- import Control.Arrow.Machine
-- import Network.HTTP.Client
-- import Network.HTTP.Client.TLS
-- import Network.HTTP.Simple

-- import Control.Monad.Trans.Resource

-- downloadBase :: String
-- downloadBase = "https://interfacelift.com"

-- randomWallpaperUrl :: String
-- randomWallpaperUrl = "https://interfacelift.com/wallpaper/downloads/random/wide_16:10/2560x1600/"

-- downloadWallpaperList :: (MonadResource m) => ProcessA (Kleisli m) (Event (Request, Manager)) (Event String)
-- downloadWallpaperList = makeRequest >>> sourceHttp_ >>> evMap (unpack . decodeUtf8 . snd)

-- machFold :: ArrowApply cat => ProcessA cat (Event String) (Event String)
-- machFold = proc input -> do
--   res <- accum mempty -< (<>) <$> input
--   ed <- onEnd -< input
--   returnA -< res <$ ed

-- parsePage :: MonadIO m => ProcessA (Kleisli m) (Event String) (Event String)
-- parsePage = constructT kleisli0 $ do
--   str <- await
--   wallpaperUrls <- lift . liftIO $ runX $ readString [withValidate no, withParseHTML yes, withWarnings no, withHTTP mempty] str
--     //> hasAttrValue "id" (isPrefixOf "download")
--     /> hasName "a"
--     >>> getAttrValue "href"
--   mapM_ (\wpUrl -> yield $ downloadBase <> wpUrl) $ take 1 $ wallpaperUrls

-- initialize :: (MonadThrow m, MonadIO m) => ProcessA (Kleisli m) (Event ()) (Event (Request, Manager))
-- initialize = constructT kleisli0 $ do
--   _ <- await
--   req <- lift $ setRequestHeaders ifaceRequestHeaders <$> parseUrlThrow randomWallpaperUrl
--   mgr <- lift . liftIO $ newManager tlsManagerSettings
--   yield (req, mgr)

-- getImageUrl :: MonadResource m => ProcessA (Kleisli m) (Event ()) (Event String)
-- getImageUrl = initialize >>> downloadWallpaperList >>> machFold >>> parsePage

-- ifaceRequestHeaders = [ ("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.98 Safari/537.36")
--                       , ("Accept", "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8")
--                       ]

getImageUrl :: IO ()
getImageUrl = putStrLn "Interface Lift is not longer supported!"
