{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (join)
import Data.Bifunctor as Bifunctor
import Data.Monoid ((<>))
import qualified Data.SemVer as SemVer
import qualified Data.Text   as Text
import qualified Data.Text.IO as TextIO
import Data.Text (Text)
import GHC.IO.Handle (hGetContents)
import System.IO.Error (catchIOError)
import System.Exit (ExitCode)
import System.Process.Text (readProcessWithExitCode)
import Prelude

pscPackageMinVersion = SemVer.version 0 2 0 [] []

main :: IO ()
main = do
  pscPackageVersion <- getPscPackageVersion
  case pscPackageVersion of
    Left err -> TextIO.putStrLn err
    Right version -> do
      print version
      if version >= pscPackageMinVersion
        then putStrLn "all good"
        else putStrLn "min version unmet"
  return ()

getPscPackageVersion :: IO (Either Text SemVer.Version)
getPscPackageVersion = join . fmap parseSemVer <$> runPscPackageCmd "sources"
  where
    parseSemVer :: Text -> Either Text SemVer.Version
    parseSemVer = Bifunctor.first Text.pack . SemVer.fromText

getPscPackageSources :: IO (Either Text [Text])
getPscPackageSources = fmap Text.lines <$> runPscPackageCmd "sources"

runPscPackageCmd :: String -> IO (Either Text Text)
runPscPackageCmd cmd =
  fmap (\(_, out, _) -> Text.stripEnd out) <$>
    catchIOError (Right <$> run) handleErr
  where
    run :: IO (ExitCode, Text, Text)
    run = readProcessWithExitCode "psc-package" ["--" ++ cmd] ""

    handleErr :: IOError -> IO (Either Text (ExitCode, Text, Text))
    handleErr err = return . Left $ "Error running psc-package: " <> Text.pack (show err)
