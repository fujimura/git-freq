{-# LANGUAGE OverloadedStrings #-}
module Git.Freq where

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
incrementChange result (fileName,numStat@(a,d)) =
    case Map.lookup fileName result of
        Just (a',d') -> Map.insert fileName (a+a', d+d') result
        Nothing      -> Map.insert fileName numStat result

render :: Change -> IO ()
render (fileName,(added,deleted)) =
    T.putStrLn . T.pack . mconcat $ [T.unpack fileName, ",",  show added, ",", show deleted]

sortResult :: [Change] -> [Change]
sortResult = let f (_,(xa,xd)) (_,(ya,yd)) = (ya+yd) `compare` (xa+xd) in sortBy f

-- TODO: This ignores the file which had many changes but deleted.
ignoreJustAdded :: [Change] -> [Change]
ignoreJustAdded [] = []
ignoreJustAdded (x@(_,(a,d)):xs)
    | a == d    = ignoreJustAdded xs
    | d == 0    = ignoreJustAdded xs
    | otherwise = x:ignoreJustAdded xs

freq :: IO ()
freq = do
    (_,out,_,_) <- runInteractiveProcess "git" ["log"
                                               , "--numstat"
                                               , "--pretty=\"%0\""
                                               ] Nothing Nothing
    freq' out >>= mapM_ render

freq' :: Source a => a -> IO [Change]
freq' source = do
    changes <- walk source []
    return $ ignoreJustAdded . sortResult . Map.toList $ sumChanges changes
