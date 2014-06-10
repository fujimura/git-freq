module Main where

import           Git.Freq
import qualified Git.Freq.Version    as Version
import           Options.Applicative


data Config = Config
  { version :: Bool }

config :: Parser Config
config = Config
  <$> switch
      (long "version" <> short 'v' <> help "Show version")
main :: IO ()
main = do
    (Config showVersion) <- execParser opts
    if showVersion then putStrLn Version.version
                   else freq
  where
    opts = info (helper <*> config)
      ( fullDesc
     -- <> progDesc "Show frequently changed code in the repository"
     <> header "git-freq - Show frequently changed code in the repository")
