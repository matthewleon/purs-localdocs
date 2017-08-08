{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad (void, join, unless)
import Control.Monad.Except (ExceptT(ExceptT), runExceptT, catchError, throwError, liftIO)
import Data.Bifunctor as Bifunctor
import Data.Either (rights)
import Data.Maybe (mapMaybe)
import Data.Monoid ((<>))
import Data.Traversable (traverse)
import qualified Data.SemVer as SemVer
import qualified Data.Text   as Text
import qualified Data.Text.Lazy   as TextL
import qualified Data.Text.IO as TextIO
import qualified Data.Text.Lazy.IO as TextLIO
import Data.Text (Text)
import GHC.IO.Handle (hGetContents)
import Safe (headMay)
import System.FilePath.Glob (glob)
import System.IO (FilePath, IOMode(ReadMode), withFile, stderr)
import System.IO.Error (catchIOError)
import System.Exit (ExitCode, exitFailure)
import System.Process.Text (readProcessWithExitCode)
import Prelude

pscPackageMinVersion = SemVer.version 0 2 0 [] []

main :: IO ()
main = void . runExceptT $ do
  checkPscPackageVersion
  paths <- ExceptT getPscPackageSourcePaths
  moduleNames <- liftIO $ rights <$> traverse getModuleName paths
  liftIO . TextIO.putStr $ Text.unlines moduleNames
  `catchError` \err -> liftIO $ TextIO.hPutStrLn stderr err >> exitFailure

checkPscPackageVersion :: ExceptT Text IO ()
checkPscPackageVersion = do
  pscPackageVersion <- ExceptT getPscPackageVersion
  unless (pscPackageVersion >= pscPackageMinVersion) $
    throwError $
      "purs-localdocs requires a Minimum psc-package version of "
      <> SemVer.toText pscPackageMinVersion
      <> "\nYou are running version " <> SemVer.toText pscPackageVersion

getPscPackageSourcePaths :: IO (Either Text [FilePath])
getPscPackageSourcePaths = traverse globAll =<< getPscPackageSources
  where
  globAll :: [Text] -> IO [FilePath]
  globAll = fmap concat . traverse (glob . Text.unpack)

getPscPackageVersion :: IO (Either Text SemVer.Version)
getPscPackageVersion = join . fmap parseSemVer <$> runPscPackageCmd "--version"
  where
  parseSemVer :: Text -> Either Text SemVer.Version
  parseSemVer t = flip Bifunctor.first (SemVer.fromText t) $ \err ->
    "Error parsing version from psc-package: " <> Text.pack err

getPscPackageSources :: IO (Either Text [Text])
getPscPackageSources = fmap Text.lines <$> runPscPackageCmd "sources"

runPscPackageCmd :: String -> IO (Either Text Text)
runPscPackageCmd cmd =
  fmap (\(_, out, _) -> Text.stripEnd out) <$>
    catchIOError (Right <$> run) handleErr
  where
  run :: IO (ExitCode, Text, Text)
  run = readProcessWithExitCode "psc-package" [cmd] ""

  handleErr :: IOError -> IO (Either Text (ExitCode, Text, Text))
  handleErr err = return . Left $
    "Error running psc-package: " <> Text.pack (show err)

getModuleName :: FilePath -> IO (Either Text Text)
getModuleName fp = withFile fp ReadMode $ \h -> runExceptT $ do
  fileContent <- ExceptT $ evaluate =<<
    catchIOError (Right <$> TextLIO.hGetContents h) handleErr
  case force $ extractModuleName fileContent of
    Nothing   -> throwError $ "Unable to read module name from " <> Text.pack fp
    Just name -> return name
  where
  handleErr :: IOError -> IO (Either Text TextL.Text)
  handleErr err = return . Left $ "Error reading content of " <> Text.pack fp
                               <> ": " <> Text.pack (show err)
  extractModuleName :: TextL.Text -> Maybe Text
  extractModuleName =
    headMay . mapMaybe (moduleNameFromLine . TextL.toStrict) . TextL.lines
    where
    moduleNameFromLine :: Text -> Maybe Text
    moduleNameFromLine =
      fmap (Text.takeWhile (`notElem` ['_', ' '])) . Text.stripPrefix "module "
