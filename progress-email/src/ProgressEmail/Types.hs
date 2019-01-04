{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ProgressEmail.Types where

import ClassyPrelude
import Data.Aeson
import qualified Data.Text.Lazy as TL
import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Data.Binary

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

data Ticket a = Ticket
  { ticketNumber :: Int
  , ticketDescription :: TL.Text
  , ticketRelations :: a
  , ticketAssignedUser :: Maybe TL.Text
  , ticketStatus :: TicketStatus
  } deriving (Show, Generic, Functor, Foldable, Traversable)

instance ToJSON a => ToJSON (Ticket a)
instance FromJSON a => FromJSON (Ticket a)

newtype TicketStatus = TicketStatus { unTicketStatus :: Text }
  deriving (Show, Generic, Eq, Ord, NFData, Binary)

instance ToJSON TicketStatus
instance FromJSON TicketStatus

newtype Email = Email TL.Text
  deriving Show

data Release a = Release
  { releaseName :: Text
  , releaseData :: a
  } deriving (Show, Generic, Functor, Foldable, Traversable)

instance ToJSON a => ToJSON (Release a)
instance FromJSON a => FromJSON (Release a)

-- deriving instance Generic (TicketF a)
-- deriving instance Show a => Show (TicketF a)

type TicketF = Fix Ticket
type ReleaseF = Fix Release
type ProgressF = Fix Progress
