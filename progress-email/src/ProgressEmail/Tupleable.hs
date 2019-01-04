{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module ProgressEmail.Tupleable where

import ProgressEmail.Types
import ClassyPrelude

import ProjectM36.Tuple
import ProjectM36.Atom
import ProjectM36.Relation
import ProjectM36.Attribute as A
import ProjectM36.Tupleable
import ProjectM36.Base
import ProjectM36.Atomable
import Data.Proxy
import qualified Data.Text.Lazy as TL

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

ticketSchema :: forall a. Atomable a => Proxy a -> [DatabaseContextExpr]
ticketSchema _ = [ toDefineExpr (Proxy :: Proxy (Ticket a)) "ticket" ]

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

instance Atomable a => Tupleable (Release a)

releaseSchema :: forall a. Atomable a => Proxy a -> [DatabaseContextExpr]
releaseSchema _ = [ toDefineExpr (Proxy :: Proxy (Release a)) "release" ]

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

runTransaction :: Db a -> IO (Either DbError a)
runTransaction = fmap ClassyPrelude.join . withDatabase (CrashSafePersistence "/tmp/test.db") . flip withTransaction

insertTickets :: (Traversable t, Atomable a) => t (Ticket a) -> IO (Either RelationalError (Either DbError ()))
insertTickets = mapM runTransaction . fmap execute . insertTickets_

retrieveTickets :: Db [Either RelationalError (Ticket [Int])]
retrieveTickets = fmap fromTuple . relFold cons mempty <$> query (RelationVariable "ticket" ())

retrieveTupleable :: Tupleable a => RelationalExpr -> Db [Either RelationalError a]
retrieveTupleable expr = fmap fromTuple . relFold cons mempty <$> query expr

retrieveRelease :: RelationalExpr -> Db [Either RelationalError (Release [Int])]
retrieveRelease = retrieveTupleable

retrieveTicket :: RelationalExpr -> Db [Either RelationalError (Ticket [Int])]
retrieveTicket = retrieveTupleable

releaseTable :: (RelationalExprBase () -> RelationalExprBase ()) -> RelationalExpr
releaseTable expr = expr $ RelationVariable "release" ()

ticketTable :: (RelationalExprBase () -> RelationalExprBase ()) -> RelationalExpr
ticketTable expr = expr $ RelationVariable "ticket" ()

-- Should be moved out of here!

tickets :: [Ticket [Int]]
tickets = [ Ticket 23857 "PIA Wave: Error While Executing EPC_SP_ADD_FCDL_FINAN_TRNS_TBL_FY Stored Procedure" [] Nothing (TicketStatus "Done")
          , Ticket 23832 "Blank Cells in the Amount to be Committed (post-discount) column in General Manager Review." [] Nothing (TicketStatus "Done")
          , Ticket 23620 "Transaction Timeouts and High Transaction Times in Window Tests" [] Nothing (TicketStatus "Done")
          , Ticket 23949 "COMAD Wave Taking Longer than Usual" [] Nothing (TicketStatus "Done")
          , Ticket 24017 "InvalidUserException Encountered in PC Review" [] Nothing (TicketStatus "Done")
          , Ticket 24014 "Query batching issue with EPC_getFrnReviewNotes() for SPIN Changes" [] Nothing (TicketStatus "Done")
          , Ticket 23956 "Tech Debt: GROUP BY in views Causing Performance drops & Timeouts in production" [] Nothing (TicketStatus "Done")
          , Ticket 23702 "E2E UAT Bug - Exception 17 Did Not Auto Clear"  [] Nothing (TicketStatus "Done")
          , Ticket 23168 "Tech Debt: Refactor Nightly Exception 5 process to improve performance"  [] Nothing (TicketStatus "Done")
          , Ticket 22486 "Tech Debt: SP - EPC_SP_PC_REV_VALIDATE_DISCOUNT is timing out in Prod"  [] Nothing (TicketStatus "Done")
          , Ticket 19966 "Tech Debt: Refactor Slow Performing Views"  [] Nothing (TicketStatus "Done")
          , Ticket 17887 "Technical Debt: Improve performance of EPC_FRN_CASE_ASSOCIATED_FRNS_VIEW"  [] Nothing (TicketStatus "Done")
          , Ticket 13724 "Tech Debt: Increase Performance of EPC_ESCALATION_REPORT_MGR_VIEW (and FY)"  [] Nothing (TicketStatus "Done")
          ]

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
getTicketRelations = fmap ClassyPrelude.join . traverse (retrieveTicket . ticketTable . ticketByNumber) . ticketRelations

