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
  where
    parse :: ByteString -> Maybe NumStat
    parse bs = case Parser.parseByteString numstat mempty bs of
      Parser.Success n -> n
      Parser.Failure doc -> error $ show doc

update :: Result -> NumStat -> Result
update result (fileName,delta,Just oldFileName) = swap oldFileName fileName $ Map.alter (incr delta) oldFileName result
update result (fileName,delta,Nothing)          = Map.alter (incr delta) fileName result

incr :: Delta -> Maybe Delta -> Maybe Delta
incr d Nothing      = Just d
incr d (Just delta) = Just $ delta <> d

swap :: Ord k => k -> k -> Map k a -> Map k a
swap old new m = case Map.lookup old m of
  Just v -> (Map.insert new v . Map.delete old) m
  Nothing -> m

sortResult :: [Change] -> [Change]
sortResult = let f (_,d1) (_,d2) = (d2.added+d2.deleted) `compare` (d1.added+d1.deleted) in sortBy f

render :: Change -> IO ()
render (fileName,delta) =
    T.putStrLn . T.pack . mconcat $ [T.unpack fileName, ",",  show delta.added, ",", show delta.deleted]
