module Types where

import Data.Text (Text)
import Data.Map.Strict (Map)

type FileName = Text
type NumStat = (FileName, Int, Int, Maybe FileName)
data Delta = Delta
  { added::Int
  , deleted:: Int
  } deriving (Show, Eq, Ord)
type Change = (FileName, Delta)
type Result = Map FileName Delta