{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE FlexibleContexts #-}

module Git.Freq where

import           Data.ByteString    (ByteString)
import           Data.List          (sortBy)
import qualified Data.Map.Strict    as Map
import           Data.Map.Strict    (Map)
import qualified Data.Text          as T
import qualified Data.Text.IO       as T
import qualified Text.Trifecta.Parser as Parser
import qualified Text.Trifecta.Result as Parser

import           System.IO.Streams  (InputStream)
import qualified System.IO.Streams  as Streams

import Types
import Git.NumStat(numstat)

freq :: [FilePath] -> IO ()
freq paths = do
    r <- getNumStatStream paths >>= freq'
    mapM_ render (sortResult $ Map.toList r)

getNumStatStream :: [FilePath] -> IO (InputStream ByteString)
getNumStatStream paths = do
  let args = ["log"
             , "--numstat"
             , "--pretty="
             , "--reverse"
             ] ++ paths

  (_,is,_,_) <- Streams.runInteractiveProcess "git" args Nothing Nothing
  return is

freq' :: InputStream ByteString -> IO Result
freq' is = Streams.lines is >>=
           Streams.map parse >>=
           Streams.mapMaybe id >>=
           Streams.fold update Map.empty

parse :: ByteString -> Maybe NumStat
parse bs = case Parser.parseByteString numstat mempty bs of
  Parser.Success n -> n
  Parser.Failure doc -> error $ show doc

update :: Result -> NumStat -> Result
update result (fileName,delta,mNewFileName) =
  let result' = Map.alter (incr Changes { delta = delta, commits = 1 }) fileName result in
  case mNewFileName of
    Just newFileName -> swap fileName newFileName result'
    Nothing -> result'

incr :: Changes -> Maybe Changes -> Maybe Changes
incr d = maybe (Just d) (\x -> Just (x <> d))

swap :: Ord k => k -> k -> Map k a -> Map k a
swap old new m = case Map.lookup old m of
  Just v -> (Map.insert new v . Map.delete old) m
  Nothing -> m

sortResult :: [(FileName, Changes)] -> [(FileName, Changes)]
sortResult = let f (_,x) (_,y) = x.delta `compare` y.delta in sortBy f

render :: (FileName, Changes) -> IO ()
render (fileName,Changes { delta = delta, commits = commits }) =
    T.putStrLn . T.pack . mconcat $ [T.unpack fileName, ",",  show delta.added, ",", show delta.deleted, ",", show commits]
