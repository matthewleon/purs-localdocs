{-# LANGUAGE OverloadedStrings #-}

module Main where

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
getPscPackageVersion = do
  cmdResult <- catchIOError (Right <$> runPscPackage) handleErr
  return $ do
    (_, stdOut, stdErr) <- cmdResult
    flip Bifunctor.first (SemVer.fromText (Text.stripEnd stdOut)) $
      \parseError -> Text.pack parseError <>
        if Text.null stdErr
        then ""
        else "\npsc-package error: " <> stdErr
  where
    runPscPackage :: IO (ExitCode, Text, Text)
    runPscPackage = readProcessWithExitCode "psc-package" ["--version"] ""

    handleErr :: IOError -> IO (Either Text (ExitCode, Text, Text))
    handleErr err = return . Left $ "Error running psc-package: " `Text.append` Text.pack (show err)
