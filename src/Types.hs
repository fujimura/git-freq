{-# LANGUAGE OverloadedRecordDot #-}

module Types where

import Data.Text (Text)
import Data.Map.Strict (Map)

type FileName = Text
data Delta = Delta
  { added::Int
  , deleted:: Int
  } deriving (Show, Eq)

instance Semigroup Delta where
  x <> y = Delta { added = x.added + y.added, deleted = x.deleted + y.deleted }

instance Monoid Delta where
  mempty        = Delta { added = 0, deleted = 0 }

instance Ord Delta where
  x `compare` y = (x.added + x.deleted) `compare` (y.added + y.deleted)

type NumStat = (FileName, Delta, Maybe FileName)

data Changes = Changes
  { delta :: Delta
  , commits :: Int
  } deriving (Show, Eq)

instance Semigroup Changes where
  x <> y = Changes { delta = x.delta <> y.delta, commits = x.commits + y.commits }

instance Monoid Changes where
  mempty = Changes { delta = Delta { added = 0, deleted = 0 }, commits = 0 }

type Result = Map FileName Changes