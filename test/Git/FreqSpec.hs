{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Git.FreqSpec (spec) where

import           Control.Exception       (catch)
import           Data.ByteString         (ByteString)
import qualified Data.ByteString.Char8   as BS
import qualified Data.Map.Strict         as Map
import           Data.String.Interpolate (i)
import           System.Exit             (ExitCode (ExitSuccess))
import           System.IO               (stdout)
import           System.IO.Silently      (capture, hSilence)
import           System.IO.Streams       (InputStream)
import qualified System.IO.Streams       as Streams
import           System.Process          (system)

import           Test.Hspec

import           Git.Freq                (freq, freq')
import           Types
import           Helper                  (commitFile, inTempRepo)

createMockStream :: [String] -> IO (InputStream ByteString)
createMockStream = Streams.fromByteString . BS.pack . unlines

spec :: Spec
spec = do
  describe "freq" $ do
    around_ (hSilence [stdout] . inTempRepo) $ do

      it "should summarize changes" $ do
        commitFile "foo.hs" [i|
                            putStrLn "Foo"
                            |]

        commitFile "foo.hs" [i|
                            putStrLn "Foo"
                            putStrLn "Bar"
                            |]

        commitFile "foo.hs" [i|
                            putStrLn "Bar"
                            |]

        system "git mv foo.hs bar.hs"
        system "git commit -a -m 'moved'"
        system "git log --numstat"

        let run paths = fst <$> capture (freq paths `catch` (\ExitSuccess -> return ()))

        run ["."] `shouldReturn` unlines ["bar.hs,2,1,4"]

  describe "freq'" $ do
    it "should summarize changes" $ do
      source <-
        createMockStream
          [ "100\t0\tgit-freq.cabal",
            "20\t10\tgit-freq.cabal",
            "0\t120\tgit-freq.cabal",
            "2\t5\tREADME.md",
            --"4\t\t3\tfoo",
            "2\t9\tREADME.md"
          ]
      freq' source
        `shouldReturn` Map.fromList
          [ ("git-freq.cabal", Changes { delta = Delta {added =120, deleted =130}, commits = 3 }),
            ("README.md", Changes { delta = Delta {added = 4, deleted =14 }, commits = 2 })
          ]
