module Main where

import           Git.Freq
import qualified Git.Freq.Version    as Version
import           Options.Applicative


data Config = Config
  { version :: Bool
  , paths :: [FilePath]
  }

config :: Parser Config
config = Config
  <$> switch
      (long "version" <> short 'v' <> help "Show version")
  <*> many
      (argument str (metavar "PATH..." <> help "Target paths"))

main :: IO ()
main = do
    (Config showVersion paths) <- execParser opts
    if showVersion then putStrLn Version.version
                   else freq paths
  where
    opts = info (helper <*> config)
      ( fullDesc
      <> progDesc (unlines [ "Total addition, deletion per file will be shown as a csv in following format:"
                            , "`file name, addition, deletion`"
                            ])

     <> header "git-freq - Show frequently changed code in the repository")
