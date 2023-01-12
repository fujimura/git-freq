module Git.NumStat (numstat) where

import           Control.Applicative
import qualified Data.Text            as T
import           Text.Trifecta         hiding (spaces)

import Types

numstat :: Parser NumStat
numstat = do
  added <- read <$> manyTill digit tab
  skipMany tab
  deleted <- read <$> manyTill digit tab
  skipMany tab
  fm <- optional (try filemoveWithBrace <|> try filemove)
  case fm of
    Just (old, current) -> return (T.pack current, added,deleted, Just $ T.pack old)
    Nothing -> do
      current <- manyTill anyChar eof
      return (T.pack current, added,deleted,  Nothing)

filemoveWithBrace :: Parser (String, String)
filemoveWithBrace = do
  before <- manyTill anyChar (char '{')
  from <-  manyTill anyChar space
  skipSome $ string "=> "
  to <-  manyTill anyChar (char '}')
  after <- manyTill anyChar eof

  return (before ++ from ++ after, before ++ to ++ after)

filemove :: Parser (String, String)
filemove = do
  from <-  manyTill anyChar space
  skipSome $ string "=> "
  to <- manyTill anyChar eof

  return (from, to)