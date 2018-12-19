{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module ProgressEmail.Types where

import ClassyPrelude
import Data.Aeson
import qualified Data.Text.Lazy as TL

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

data Message body tickets = Message
  { messageBody :: body
  , messageTickets :: tickets
  } deriving (Show, Generic, Functor, Foldable, Traversable)

instance (ToJSON body, ToJSON tickets) => ToJSON (Message body tickets)
instance (FromJSON body, FromJSON tickets) => FromJSON (Message body tickets)

data Ticket = Ticket
  { ticketNumber :: Int
  , ticketDescription :: TL.Text
  } deriving (Show, Generic)

instance ToJSON Ticket
instance FromJSON Ticket

newtype Email = Email TL.Text
  deriving Show

