{-# LANGUAGE OverloadedStrings #-}
module Git.Freq where

import           Control.Arrow   ((***))
import           Data.List       (sortBy)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Monoid
import           Data.Text       (Text)
import qualified Data.Text       as T
import qualified Data.Text.IO    as T
import           System.Process  (runInteractiveProcess)
import           Text.Read       (readMaybe)

import           Git.Freq.Source (Source)
import qualified Git.Freq.Source as Source

type FileName = Text
type NumStat = (Int, Int)
type Change = (FileName, NumStat)
type Result = Map FileName NumStat

parseLine :: Text -> Maybe Change
parseLine = go . T.splitOn (T.pack "\t")
    where go :: [Text] -> Maybe Change
          go (_:_:"":_) = Nothing
          go (added:deleted:filename:_) =
            case (readIntMaybe added, readIntMaybe deleted) of
                (Just a, Just d) -> Just (filename, (a, d))
                _                -> Nothing
          go _ = Nothing
          readIntMaybe x = readMaybe (T.unpack x) :: Maybe Int

walk :: Source a => a -> [Change] -> IO [Change]
walk h changes = do
    eof <- Source.isEOF h
    if eof then return changes
           else do
             l <- Source.getLine h
             case parseLine l of
               Just c  -> walk h (c:changes)
               Nothing -> walk h changes

sumChanges :: [Change] -> Result
sumChanges = foldl incrementChange Map.empty

incrementChange :: Result -> Change -> Result
incrementChange result (fileName,numstat@(a,d)) = Map.alter f fileName result
  where
    f numstat' = Just $ maybe numstat ((a+) *** (d+)) numstat'

render :: Change -> IO ()
render (fileName,(added,deleted)) =
    T.putStrLn . T.pack . mconcat $ [T.unpack fileName, ",",  show added, ",", show deleted]

sortResult :: [Change] -> [Change]
sortResult = let f (_,(xa,xd)) (_,(ya,yd)) = (ya+yd) `compare` (xa+xd) in sortBy f

freq :: [FilePath] -> IO ()
freq paths = do
    let args = ["log"
               , "--numstat"
               , "--pretty=\"%0\""
               ] ++ paths

    (_,out,_,_) <- runInteractiveProcess "git" args Nothing Nothing
    freq' out >>= mapM_ render

freq' :: Source a => a -> IO [Change]
freq' source = do
    changes <- walk source []
    return $ sortResult . Map.toList $ sumChanges changes
