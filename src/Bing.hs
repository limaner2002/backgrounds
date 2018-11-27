{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bing
  ( getImageUrl ) where

import ClassyPrelude

-- import Text.XML.HXT.Core
-- import Text.XML.HXT.HTTP
-- import Control.Arrow.Machine

market = "en-US"
resolution = "1920x1080"
bingUrl = "http://www.bing.com"
ext = ".jpg"

-- getUrlBase :: ArrowXml cat => cat XmlTree String
-- getUrlBase = hasName "images" /> hasName "image" /> hasName "urlBase" /> getText

-- imageUrl :: String -> String
-- imageUrl urlBase = url -- (url, takeFileName url)
--   where
--     url = bingUrl <> urlBase <> "_" <> resolution <> ext


-- getImageUrl :: MonadIO m => ProcessA (Kleisli m) (Event ()) (Event String)
-- getImageUrl = constructT kleisli0 go
--   where
--     go = do
--       urlBases <- lift . liftIO $ runX (readDocument [withValidate no, withHTTP mempty] ("http://www.bing.com/HPImageArchive.aspx?format=xml&idx=0&n=1&mkt=" <> market) /> getUrlBase)
--       loop urlBases
--     loop [] = return ()
--     loop (x:xs) = do
--       yield $ imageUrl x
--       loop xs

getImageUrl :: IO ()
getImageUrl = error "No longer implemented!"
