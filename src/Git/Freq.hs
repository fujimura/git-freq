{-# LANGUAGE OverloadedStrings #-}
module Git.Freq where

import           Control.Arrow      ((***))
import           Data.ByteString    (ByteString)
import           Data.List          (sortBy)
import qualified Data.Map.Strict    as Map
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
             ] ++ paths

  (_,is,_,_) <- Streams.runInteractiveProcess "git" args Nothing Nothing
  return is

freq' :: InputStream ByteString -> IO Result
freq' is = Streams.lines is >>=
           Streams.map parse >>=
           Streams.mapMaybe id >>=
           Streams.fold incrementChange Map.empty
  where
    parse :: ByteString -> Maybe NumStat
    parse bs = case Parser.parseByteString numstat mempty bs of
      Parser.Success n -> Just n
      Parser.Failure doc -> error $ show doc

incrementChange :: Result -> NumStat -> Result
incrementChange result (fileName,a,d,_) = Map.alter f fileName result
  where
    f numstat' = Just $ maybe (a,d) ((a+) *** (d+)) numstat'

sortResult :: [Change] -> [Change]
sortResult = let f (_,(xa,xd)) (_,(ya,yd)) = (ya+yd) `compare` (xa+xd) in sortBy f

render :: Change -> IO ()
render (fileName,(added,deleted)) =
    T.putStrLn . T.pack . mconcat $ [T.unpack fileName, ",",  show added, ",", show deleted]
