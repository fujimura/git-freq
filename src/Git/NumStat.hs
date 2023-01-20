module Git.NumStat (numstat) where

import           Control.Applicative
import qualified Data.Text            as T
import           Text.Trifecta         hiding (spaces)
import Text.Parser.LookAhead (LookAheadParsing(lookAhead))

import Types

numstat :: Parser (Maybe NumStat)
numstat = do
  hyphen <- lookAhead $ optional (char '-')
  case hyphen of
    Just _ -> return Nothing
    Nothing -> do
      added <- read <$> manyTill digit tab
      skipMany tab
      deleted <- read <$> manyTill digit tab
      skipMany tab
      fm <- optional (try filemoveWithBrace <|> try filemove)
      case fm of
        Just (old, current) -> return $ Just (T.pack old, Delta { added = added, deleted = deleted } , Just $ T.pack current)
        Nothing -> do
          current <- manyTill anyChar eof
          return $ Just (T.pack current, Delta { added = added, deleted = deleted },  Nothing)

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