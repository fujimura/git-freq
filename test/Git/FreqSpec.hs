{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
module Git.FreqSpec ( spec ) where

import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict       as Map
import           System.IO.Streams     (InputStream)
import qualified System.IO.Streams     as Streams
import           Test.Hspec

import           Git.Freq

createMockStream :: [String] -> IO (InputStream ByteString)
createMockStream = Streams.fromByteString . BS.pack . unlines

spec :: Spec
spec = do
    describe "freq'" $ do
      it "should summarize changes" $ do
        source <- createMockStream [ "100\t0\tgit-freq.cabal"
                                   , "20\t10\tgit-freq.cabal"
                                   , "0\t120\tgit-freq.cabal"
                                   , "2\t5\tREADME.md"
                                   , "%0"
                                   , "4\t3\t"
                                   , "4\t\t3\tfoo"
                                   , "2\t9\tREADME.md"
                                   ]
        freq' source `shouldReturn` Map.fromList [ ("git-freq.cabal", (120, 130))
                                                 , ("README.md", (4, 14))
                                                 ]
