{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Git.NumStatSpec (spec, main) where

import           Data.Text            (Text)
import qualified Data.Text            as T
import           Git.NumStat          (numstat)
import           Test.Hspec
import           Text.Trifecta.Parser (Parser)
import qualified Text.Trifecta.Parser as Parser
import           Text.Trifecta.Result (Result (Failure, Success))
import           Types

parse :: Parser a -> Text -> a
parse p t = case Parser.parseString p mempty (T.unpack t) of
  Success x   -> x
  Failure doc -> error (show doc)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "numstat" $ do
  it "should parse" $ do
    parse numstat "0\t0\tfoo"
      `shouldBe` Just ("foo", Delta { added = 0, deleted = 0} , Nothing)
    parse numstat "10\t20\tfoo/{Git => src/Git}"
      `shouldBe` Just ("foo/Git", Delta { added = 10, deleted = 20 }, Just "foo/src/Git")
    parse numstat "0\t0\t.foo => .bar"
      `shouldBe` Just (".foo", Delta { added = 0, deleted = 0 }, Just ".bar")
    parse numstat "1\t\t0\t\ta"
      `shouldBe` Just ("a", Delta { added = 1, deleted = 0 }, Nothing)
    parse numstat "-\t\t-\t\tbinary.png"
      `shouldBe` Nothing
