{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module ProgressEmail.Tupleable where

import ProgressEmail.Types
import ClassyPrelude

import ProjectM36.Tuple
import ProjectM36.Atom
import ProjectM36.Relation hiding (toList)
import ProjectM36.Attribute as A
import ProjectM36.Tupleable
import ProjectM36.Base
import ProjectM36.Atomable
import ProjectM36.Key
import Data.Proxy
import qualified Data.Text.Lazy as TL
import Data.Binary

import ProjectM36.Client.Simple
import ProjectM36.Client (EvaluatedNotification)

instance Tupleable a => Tupleable (Progress a) where
  toTuple progress =
    mkRelationTuple (toAttributes (Proxy :: Proxy (Progress a))) atomVector

    where
      dataTuples = pure $ toTuple $ message progress
      relsFromData = case mkRelationFromTuples (toAttributes (Proxy :: Proxy a)) dataTuples of
        Left err -> error (show err)
        Right rel -> rel
      atomVector = fromList [ IntAtom (totalPlanned progress)
                            , IntAtom (failed progress)
                            , IntAtom (passed progress)
                            , IntAtom (executedToday progress)
                            , IntAtom (inProgress progress)
                            , RelationAtom relsFromData
                            ]
  fromTuple tupIn = do
    totalPlannedAtom <- atomForAttributeName "totalPlanned" tupIn
    failedAtom <- atomForAttributeName "failed" tupIn
    passedAtom <- atomForAttributeName "passed" tupIn
    executedTodayAtom <- atomForAttributeName "executedToday" tupIn
    inProgressAtom <- atomForAttributeName "inProgress" tupIn
    messageAtom <- atomForAttributeName "message" tupIn
    messageData <- relationForAtom messageAtom
    a <- maybe (fail "Could not convert the relation!") fromTuple $ oneTuple messageData
    pure $ Progress (fromAtom totalPlannedAtom) (fromAtom failedAtom) (fromAtom passedAtom) (fromAtom executedTodayAtom) (fromAtom inProgressAtom) a

  toAttributes _ = A.attributesFromList [ Attribute "totalPlanned" IntAtomType
                                        , Attribute "failed" IntAtomType
                                        , Attribute "passed" IntAtomType
                                        , Attribute "executedToday" IntAtomType
                                        , Attribute "inProgress" IntAtomType
                                        , Attribute "message" $ RelationAtomType (toAttributes (Proxy :: Proxy a))
                                        ]

instance Atomable TL.Text where
  toAtom = toAtom . toChunks
  fromAtom = fromChunks . fromAtom

  toAtomType _ = toAtomType (Proxy :: Proxy [Text])
  toAddTypeExpr _ = toAddTypeExpr (Proxy :: Proxy [Text])

instance Atomable a => Tupleable (Ticket a)
instance Atomable TicketStatus

instance Atomable TicketNumber
instance NFData TicketNumber
instance Binary TicketNumber

ticketSchema :: forall a. Atomable a => Proxy a -> [DatabaseContextExpr]
ticketSchema _ = [ toAddTypeExpr (Proxy :: Proxy TicketStatus)
                 , toAddTypeExpr (Proxy :: Proxy TicketNumber)
                 , toAddTypeExpr (Proxy :: Proxy Release)
                 , toAddTypeExpr (Proxy :: Proxy ScriptedStatus)
                 , toDefineExpr (Proxy :: Proxy (Ticket a)) "ticket"
                 , databaseContextExprForUniqueKey "ticket" ["_ticketNumber"]
                 ]

instance Tupleable a => Tupleable (MarkDown a) where
  toTuple md =
    mkRelationTuple (toAttributes (Proxy :: Proxy a)) atomVector

    where
      dataTuples = pure $ toTuple $ progress md
      relsFromData = case mkRelationFromTuples (toAttributes (Proxy :: Proxy a)) dataTuples of
        Left err -> error (show err)
        Right rel -> rel
      atomVector = fromList [ RelationAtom relsFromData
                            , toAtom (title md)
                            ]

  fromTuple tupIn = do
    progressAtom <- atomForAttributeName "progress" tupIn
    progressData <- relationForAtom progressAtom
    a <- maybe (fail "Could not convert the relation!") fromTuple $ oneTuple progressData
    titleAtom <- atomForAttributeName "title" tupIn
    pure $ MarkDown a (fromAtom titleAtom)

  toAttributes _ = A.attributesFromList [ Attribute "progress" $ RelationAtomType (toAttributes (Proxy :: Proxy a))
                                        , Attribute "title" $ toAtomType (Proxy :: Proxy TL.Text)
                                        ]

instance Atomable Release
instance NFData Release
instance Binary Release

instance Atomable Environment
instance Tupleable Deployment

instance Atomable ScriptedStatus
instance NFData ScriptedStatus
instance Binary ScriptedStatus

-- Utility

withDatabase :: PersistenceStrategy -> (DbConn -> IO a) -> IO (Either DbError a)
withDatabase persistenceStrategy = bracket connect disconnect . traverse
  where
    connect = simpleConnectProjectM36 (InProcessConnectionInfo persistenceStrategy notify [])

disconnect :: Either DbError DbConn -> IO (Either DbError ())
disconnect (Left msg) = pure $ Left msg
disconnect (Right conn) = Right <$> close conn

notify :: NotificationName -> EvaluatedNotification -> IO ()
notify name eval = putStrLn $ "name: " <> tshow name <> "\n"
  <> tshow eval
  <> "\n"

createSchema :: [DatabaseContextExpr] -> DbConn -> IO ()
createSchema ctx conn = do
  eRes <- mapM (withTransaction conn . execute) ctx
  let errs = lefts eRes
  mapM_ print errs

insertTickets_ :: (Traversable t, Atomable a) => t (Ticket a) -> Either RelationalError DatabaseContextExpr
insertTickets_ = flip toInsertExpr "ticket"

runTransaction :: FilePath -> Db a -> IO (Either DbError a)
runTransaction dbPath = fmap ClassyPrelude.join . withDatabase (CrashSafePersistence dbPath) . flip withTransaction

insertTickets :: (Traversable t, Atomable a) => FilePath -> t (Ticket a) -> IO (Either RelationalError (Either DbError ()))
insertTickets dbPath = mapM (runTransaction dbPath) . fmap execute . insertTickets_

updateTicketExpr :: forall a. Atomable a => Ticket a -> Either RelationalError DatabaseContextExpr
updateTicketExpr = toUpdateExpr "ticket" (toList $ A.attributeNames $ toAttributes (Proxy :: Proxy (Ticket a)))

updateTicket :: Atomable a => FilePath -> Ticket a -> IO (Either RelationalError (Either DbError ()))
updateTicket dbPath = sequence . fmap (runTransaction dbPath . execute) . updateTicketExpr

retrieveTickets :: Db [Either RelationalError (Ticket [Int])]
retrieveTickets = fmap fromTuple . relFold cons mempty <$> query (RelationVariable "ticket" ())

retrieveTupleable :: Tupleable a => RelationalExpr -> Db [Either RelationalError a]
retrieveTupleable expr = fmap fromTuple . relFold cons mempty <$> query expr

retrieveTicket :: RelationalExpr -> Db [Either RelationalError (Ticket [Int])]
retrieveTicket = retrieveTupleable

releaseTable :: (RelationalExprBase () -> RelationalExprBase ()) -> RelationalExpr
releaseTable expr = expr $ RelationVariable "release" ()

ticketTable :: (RelationalExprBase () -> RelationalExprBase ()) -> RelationalExpr
ticketTable expr = expr $ RelationVariable "ticket" ()

-- Should be moved out of here!

releaseByName :: Text -> RelationalExprBase a -> RelationalExprBase a
releaseByName name = Restrict $ AttributeEqualityPredicate "releaseName" (NakedAtomExpr $ toAtom name)

ticketByNumber :: Int -> RelationalExprBase a -> RelationalExprBase a
ticketByNumber num = Restrict $ AttributeEqualityPredicate "ticketNumber" (NakedAtomExpr $ toAtom num)

ticketNumberPred :: Int -> RestrictionPredicateExprBase a
ticketNumberPred = AttributeEqualityPredicate "ticketNumber" . NakedAtomExpr . toAtom

ticketByStatus :: TicketStatus -> RelationalExprBase a -> RelationalExprBase a
ticketByStatus status = Restrict $ AttributeEqualityPredicate "ticketStatus" (NakedAtomExpr $ toAtom status)

ticketStatusPred :: TicketStatus -> RestrictionPredicateExprBase a
ticketStatusPred status = AttributeEqualityPredicate "ticketStatus" (NakedAtomExpr $ toAtom status)

getTicketRelations :: Ticket [Int] -> Db [Either RelationalError (Ticket [Int])]
getTicketRelations = fmap ClassyPrelude.join . traverse (retrieveTicket . ticketTable . ticketByNumber) . _ticketRelations

ticketsByRelease :: Release -> RelationalExprBase a -> RelationalExprBase a
ticketsByRelease release = Restrict $ AttributeEqualityPredicate "_ticketRelease" (NakedAtomExpr $ toAtom release)

getTicketsByRelease :: Release -> Db [Either RelationalError (Ticket [Int])]
getTicketsByRelease release = retrieveTupleable $ ticketsByRelease release $ RelationVariable "ticket" ()

schema :: [DatabaseContextExpr]
schema =
  ticketSchema (Proxy :: Proxy [Int])
  <> [databaseContextExprForUniqueKey "ticket" ["ticketNumber"]]
