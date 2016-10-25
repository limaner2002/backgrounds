{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module NatGeo where

import ClassyPrelude

import Text.XML.HXT.Core
import Text.XML.HXT.HTTP
import Control.Arrow.Machine

baseUrl = "http://www.nationalgeographic.com/photography/photo-of-the-day/"

getImageUrl :: MonadIO m => ProcessA (Kleisli m) (Event ()) (Event String)
getImageUrl = constructT kleisli0 go
  where
    go = do
      urlBases <- lift . liftIO $ runX $ readDocument [withValidate no, withParseHTML yes, withWarnings no, withHTTP mempty] baseUrl //> hasAttrValue "property" (=="og:image") >>> getAttrValue "content"
      loop urlBases
    loop [] = return ()
    loop (x:xs) = do
      yield x
      loop xs

