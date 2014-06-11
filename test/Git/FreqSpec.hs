{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Git.FreqSpec ( spec ) where

import           Control.Applicative
import           Data.IORef
import qualified Data.Text           as T
import           Git.Freq
import           Git.Freq.Source
import           Test.Hspec

type MockSource = IORef [String]

instance Source MockSource where
    isEOF s = null <$> readIORef s
    getLine s = readIORef s >>= go
      where
        go [] = error "Empty"
        go (x:xs) = do
          writeIORef s xs
          return $ T.pack x

createMockSource :: [String] -> IO MockSource
createMockSource = newIORef

spec :: Spec
spec = do
    describe "freq'" $
      it "should summarize changes" $ do
        source <- createMockSource [ "1\t1\tgit-freq.cabal"
                                   , "2\t5\tREADME.md"
                                   , "3\t0\tgit-freq.cabal"
                                   , "%0"
                                   , "4\t3\t"
                                   , "4\t\t3\tfoo"
                                   , "2\t9\tREADME.md"
                                   ]
        freq' source `shouldReturn` [ ("README.md", (4, 14))
                                    , ("git-freq.cabal", (4, 1))
                                    ]
