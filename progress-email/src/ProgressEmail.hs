{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module ProgressEmail
  ( buildAndWriteEmail
  ) where

import ClassyPrelude
import Data.Time
import Formatting
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TL
import Data.Aeson

date :: LocalTime -> TL.Text
date = TL.pack . formatTime defaultTimeLocale "%d %b, %a"

getMessage :: FilePath -> IO Text
getMessage = readFile >=> pure . decodeUtf8

newtype Percentage = Percentage Double
  deriving Show

data Progress a = Progress
  { totalPlanned :: Int
  , failed :: Int
  , passed :: Int
  , executedToday :: Int
  , inProgress :: Int
  , message :: a
  } deriving (Show, Generic, Functor, Foldable, Traversable)

instance ToJSON a => ToJSON (Progress a)
instance FromJSON a => FromJSON (Progress a)

data MarkDown a = MarkDown
  { progress :: Progress a
  , title :: TL.Text
  } deriving (Show, Generic, Functor, Foldable, Traversable)

instance ToJSON a => ToJSON (MarkDown a)
instance FromJSON a => FromJSON (MarkDown a)

percentage :: Progress a -> Percentage
percentage progress = Percentage $ fromIntegral complete / fromIntegral (totalPlanned progress)
  where
    complete = failed progress + passed progress

dispPercentage :: Percentage -> TL.Text
dispPercentage (Percentage pct) = format (fixed 0 % "%") $ pct * 100

dispInt :: Int -> TL.Text
dispInt = format int

dispExecutionSummary :: Progress TL.Text -> TL.Text
dispExecutionSummary progress =
     "--------  ---"                         <> "\n"
  <> "Executed  " <> dispInt total  <> "\n"
  <> "Passed    " <> dispInt (passed progress) <> "\n"
  <> "Failed    " <> dispInt (failed progress) <> "\n"
  <> "--------  ---\n"
  where
    total = failed progress + passed progress

dispProgress :: Progress TL.Text -> TL.Text
dispProgress progress =
     "* Overall test execution progress is " <> dispPercentage (percentage progress) <> " complete.\n"
  <> "* Total test scenarios executed today: " <> dispInt (executedToday progress) <> "\n"
  <> "* Tests in progress: " <> dispInt (inProgress progress) <> "\n"
  <> "\n"
  <> dispExecutionSummary progress
  <> "\n\n"
  <> message progress

loadMessage :: MarkDown FilePath -> IO (MarkDown TL.Text)
loadMessage = traverse (pure . decodeUtf8 . fromStrict <=< readFile)

dispMarkDown :: LocalTime -> MarkDown TL.Text -> TL.Text
dispMarkDown currentTime md =
     date currentTime <> " " <> title md <> "\n"
  <> "\n"
  <> dispProgress (progress md)

loadMarkDown :: FilePath -> IO (Either String (MarkDown FilePath))
loadMarkDown = pure . eitherDecode . fromStrict <=< readFile

newtype Email = Email TL.Text
  deriving Show

buildEmail :: FilePath -> IO (Either String Email)
buildEmail fp = do
  currentTime <- utcToLocalTime <$> getCurrentTimeZone <*> getCurrentTime
  eMd <- loadMarkDown fp
  flip traverse eMd $ \md -> do
    email <- loadMessage md
    pure . Email $ dispMarkDown currentTime email

writeEmail :: FilePath -> Email -> IO ()
writeEmail fp (Email msg) = writeFile fp $ toStrict $ encodeUtf8 msg

buildAndWriteEmail :: FilePath -> FilePath -> IO ()
buildAndWriteEmail source dest = do
  eEmail <- buildEmail source
  case eEmail of
    Left err -> putStrLn $ pack err
    Right email -> writeEmail dest email
