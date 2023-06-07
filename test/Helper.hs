{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Helper where

import           Data.String.Interpolate.Util (unindent)
import           System.Directory             (createDirectoryIfMissing)
import           System.FilePath              (takeDirectory)
import           System.Exit                  (ExitCode(ExitFailure, ExitSuccess), exitWith)
import           System.Process               (system)
import           Test.Mockery.Directory       (inTempDirectory)

inTempRepo :: IO a -> IO a
inTempRepo action = inTempDirectory $ do
    system "git init ."
    action

commitFile :: FilePath -> String -> IO ()
commitFile path content = do
    write path content
    system "git add ."
    exitcode <- system "git commit -m \"Update\""
    case exitcode of
      ExitSuccess -> return ()
      ExitFailure _ -> exitWith exitcode
  where
    write :: FilePath -> String -> IO ()
    write path content = do
      createDirectoryIfMissing True (takeDirectory path)
      writeFile path (unindent content)