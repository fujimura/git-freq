module Git.Freq.Source
  ( Source
  , getLine
  , isEOF
  ) where

import           Data.Text    (Text)
import qualified Data.Text.IO as T
import           Prelude      hiding (getLine)
import           System.IO    (Handle)
import qualified System.IO    as IO

class Source s where
    isEOF :: s -> IO Bool
    getLine :: s -> IO Text

instance Source Handle where
    isEOF = IO.hIsEOF
    getLine = T.hGetLine
