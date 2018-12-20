{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

-- instance Atomable a => Tupleable (Progress a)

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

instance Tupleable Ticket

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

       -- It's not clear if the below typeclass is useful at all
-- class Relatable a b | b -> a where
--   fromRelatable :: a -> b

-- instance Atomable b => Relatable Atom b where
--   fromRelatable = fromAtom
