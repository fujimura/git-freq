module Types where

import Data.Text (Text)
import Data.Map.Strict (Map)

type FileName = Text
type NumStat = (FileName, Int, Int, Maybe FileName)
type Change = (FileName, (Int, Int))
type Result = Map FileName (Int, Int)