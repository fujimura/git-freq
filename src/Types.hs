{-# LANGUAGE OverloadedRecordDot #-}

module Types where

import Data.Text (Text)
import Data.Map.Strict (Map)

type FileName = Text
data Delta = Delta
  { added::Int
  , deleted:: Int
  } deriving (Show, Eq, Ord)

instance Semigroup Delta where
  x <> y = Delta { added = x.added + y.added, deleted = x.deleted + y.deleted }

instance Monoid Delta where
  mempty        = Delta { added = 0, deleted = 0 }

type NumStat = (FileName, Delta, Maybe FileName)
type Change = (FileName, Delta)
type Result = Map FileName Delta