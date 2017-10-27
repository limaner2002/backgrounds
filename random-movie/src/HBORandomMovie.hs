{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module HBORandomMovie
  ( getRandomMovie
  , movieInfo
  ) where

import ClassyPrelude
import Text.XML.HXT.Core
import Text.XML.HXT.HTTP
import Data.Aeson
import JSONTree hiding ((//>))
import qualified JSONTree as JT
import Data.Random hiding (randomElement)
import Options.Applicative hiding ((<>))
import qualified Options.Applicative as OA
import Options.Applicative.Types

decodeDoc :: MonadIO m => String -> m (Either String Value)
decodeDoc url = do
  ctnt <- liftIO $ runX $ readDocument [withParseHTML yes, withValidate no, withWarnings no, withHTTP mempty] url //> hasAttrValue "data-id" (=="reactContent") /> getText
  let mVal = fmap (eitherDecode) . stripPrefix "window.__reactContent=" . encodeUtf8 . pack . concat $ ctnt
  case mVal of
    Nothing -> return $ Left $ "Could not read the movie list from " <> url
    Just val -> return val

getMovies :: Arrow a => a (Either String Value) (Either String [Value])
getMovies = arr $ fmap (runLA $ deep_ (hasKey "availability") JT.//> getKeyValue "title")

sampleMovie :: MonadRandom m => Kleisli m (Either String [Value]) (Either String Value)
sampleMovie = Kleisli (\l -> do
                          eMVal <- mapM randomElement l
                          return $ do
                            mVal <- eMVal
                            case mVal of
                              Nothing -> Left "The movie list was empty for some reason."
                              Just v -> Right v
                      )

randomElement :: MonadRandom m => [a] -> m (Maybe a)
randomElement l = do
  n <- sample $ uniform 0 (length l - 1)
  return $ index l n

getRandomMovie :: (MonadIO m, MonadRandom m) => m (Either String Value)
getRandomMovie = runKleisli mainArr "http://www.hbo.com/schedule/hbonow-hbogo?category=PICSP932"
  where
    mainArr = Kleisli decodeDoc >>> getMovies >>> sampleMovie

movieCommands :: Parser (IO ())
movieCommands = (getRandomMovie >>= print) <$ flag' ()
  ( long "hbo"
  <> help "Pick a random movie from hbo.com."
  )

movieInfo :: ParserInfo (IO ())
movieInfo = info (helper <*> movieCommands)
  (  fullDesc
  <> progDesc "Pick a random movie from hbo.com."
  )
