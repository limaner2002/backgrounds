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
import qualified Data.Csv as Csv
import Control.Lens

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
  { _ticketNumber :: TicketNumber
  , _ticketDescription :: TL.Text
  , _ticketRelations :: a
  , _ticketAssignedUser :: Maybe TL.Text
  , _ticketStatus :: TicketStatus
  , _ticketScheduled :: [UTCTime]
  , _ticketExecuted :: [UTCTime]
  , _ticketScriptedStatus :: ScriptedStatus
  , _ticketRelease :: Release
  } deriving (Show, Generic, Functor, Foldable, Traversable)

data Deployment = Deployment
  { _deployedEnvironment :: Environment
  , _deployedDate :: UTCTime
  } deriving (Show, Generic)

newtype Environment = Environment
  { _environmentName :: Text
  } deriving (Show, Generic, Eq, NFData, Binary)

instance ToJSON Deployment
instance FromJSON Deployment

instance ToJSON Environment
instance FromJSON Environment

instance ToJSON a => ToJSON (Ticket a)
instance FromJSON a => FromJSON (Ticket a)

newtype TicketStatus = TicketStatus { _ticketStatusTxt :: Text }
  deriving (Show, Generic, Eq, Ord, NFData, Binary, Csv.FromField)

instance ToJSON TicketStatus
instance FromJSON TicketStatus

newtype Email = Email TL.Text
  deriving Show

newtype Release = Release
  { _releaseTxt :: Text
  } deriving (Show, Generic, Eq, Ord, IsString)

instance ToJSON Release
instance FromJSON Release

newtype TicketNumber = TicketNumber
  { _ticketNumberInt :: Int
  } deriving (Show, Generic, Eq, Ord, Num)

newtype JiraReleaseFilter = JiraReleaseFilter
  { _jiraFilterTicket :: Release -> Ticket [Int]
  }

instance Csv.FromNamedRecord JiraReleaseFilter where
  parseNamedRecord r = JiraReleaseFilter <$>
    (     Ticket
      <$> r Csv..: "Key"
      <*> r Csv..: "Summary"
      <*> pure mempty
      <*> pure Nothing
      <*> r Csv..: "Status"
      <*> pure mempty
      <*> pure mempty
      <*> pure NotStarted
    )

instance Csv.FromField TicketNumber where
  parseField bs = case stripPrefix "EPC-" bs of
    Nothing -> fail "Could not parse the ticket number!"
    Just stripped -> TicketNumber <$> Csv.parseField stripped

instance ToJSON TicketNumber
instance FromJSON TicketNumber

data ScriptedStatus
  = NotStarted
  | InProgress
  | Done
  deriving (Show, Eq, Ord, Generic)

instance ToJSON ScriptedStatus
instance FromJSON ScriptedStatus

makeLenses ''Ticket
makeLenses ''TicketNumber
makeLenses ''TicketStatus
makePrisms ''ScriptedStatus
