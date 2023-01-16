module Types where

import Data.Text (Text)
import Data.Map.Strict (Map)

type FileName = Text
type NumStat = (FileName, Int, Int, Maybe FileName)
type Delta = (Int, Int)
type Change = (FileName, Delta)
type Result = Map FileName Delta