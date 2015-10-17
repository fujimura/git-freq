{-# LANGUAGE OverloadedStrings #-}
module Git.Freq where

import           Control.Arrow      ((***))
import           Data.ByteString    (ByteString)
import           Data.List          (sortBy)
import           Data.Map.Strict    (Map)
import qualified Data.Map.Strict    as Map
import           Data.Text          (Text)
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO       as T
import           Text.Read          (readMaybe)

import           System.IO.Streams  (InputStream)
import qualified System.IO.Streams  as Streams

type FileName = Text
type NumStat = (Int, Int)
type Change = (FileName, NumStat)
type Result = Map FileName NumStat

freq :: [FilePath] -> IO ()
freq paths = do
    r <- getNumStatStream paths >>= freq'
    mapM_ render (sortResult $ Map.toList r)

getNumStatStream :: [FilePath] -> IO (InputStream ByteString)
getNumStatStream paths = do
  let args = ["log"
             , "--numstat"
             , "--pretty=\"%0\""
             ] ++ paths

  (_,is,_,_) <- Streams.runInteractiveProcess "git" args Nothing Nothing
  return is

freq' :: InputStream ByteString -> IO Result
freq' is = Streams.lines is >>=
           Streams.map (parseLine . T.decodeUtf8) >>=
           Streams.mapMaybe id >>=
           Streams.fold incrementChange Map.empty

sumChanges :: [Change] -> Result
sumChanges = foldl incrementChange Map.empty

incrementChange :: Result -> Change -> Result
incrementChange result (fileName,numstat@(a,d)) = Map.alter f fileName result
  where
    f numstat' = Just $ maybe numstat ((a+) *** (d+)) numstat'

sortResult :: [Change] -> [Change]
sortResult = let f (_,(xa,xd)) (_,(ya,yd)) = (ya+yd) `compare` (xa+xd) in sortBy f

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

render :: Change -> IO ()
render (fileName,(added,deleted)) =
    T.putStrLn . T.pack . mconcat $ [T.unpack fileName, ",",  show added, ",", show deleted]
