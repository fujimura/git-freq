module Main where

import           Data.Version        (showVersion)
import           Git.Freq
import           Options.Applicative
import qualified Paths_git_freq      (version)

filePaths :: Parser [FilePath]
filePaths = many (argument str (metavar "PATH..." <> help "Target paths"))

version :: Parser (a -> a)
version = infoOption (showVersion Paths_git_freq.version)
  (  short 'v'
  <> long "version"
  <> help "Print version information" )

main :: IO ()
main = execParser opts >>= freq
  where
    opts = info (helper <*> version <*> filePaths)
      ( fullDesc
      <> progDesc (unlines [ "Total addition, deletion per file will be shown as a csv in following format:"
                            , "`file name, addition, deletion`"
                            ])

     <> header "git-freq - Show frequently changed code in a repository")
