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
      `shouldBe` ("foo", 0, 0, Nothing)
    parse numstat "10\t20\tfoo/{Git => src/Git}"
      `shouldBe` ("foo/src/Git", 10, 20, Just "foo/Git")
    parse numstat "0\t0\t.foo => .bar"
      `shouldBe` (".bar", 0, 0, Just ".foo")
    parse numstat "1\t\t0\t\ta"
      `shouldBe` ("a", 1, 0, Nothing)
