{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Arrow ((>>>))
import Control.Exception.Safe (Exception, throw, tryAny, catchAny, displayException)
import qualified Control.Foldl as Fold
import Data.Char (isSpace)
import Data.Either (either, partitionEithers)
import qualified Data.List as List
import qualified Data.List.NonEmpty as NEList
import Data.Monoid (First(First), getFirst, (<>))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Foldable (traverse_)
import Data.Traversable (traverse)
import qualified Data.SemVer as SemVer
import Data.Text (Text)
import qualified Data.Text as Text
import System.FilePath.Glob (glob)
import qualified Filesystem.Path.CurrentOS as Path
import Filesystem.Path.CurrentOS ((</>), (<.>))
import Safe (headMay)

import Turtle
import Prelude hiding (FilePath)

pscPackageMinVersion = SemVer.version 0 2 0 [] []

main :: IO ()
main = do
  checkPscPackageVersion
  paths <- getPscPackageSourcePaths
  moduleNames <- getModuleNames paths
  genDocs (Set.toList paths) (Set.toList moduleNames)
  `catchAny` \e -> stderrException e >> exit (ExitFailure 1)

checkPscPackageVersion :: MonadIO m => m ()
checkPscPackageVersion = do
  pscPackageVersion <- getPscPackageVersion
  unless (pscPackageVersion >= pscPackageMinVersion) $
    liftIO . die $
      "purs-localdocs requires a minimum psc-package version of "
      <> SemVer.toText pscPackageMinVersion
      <> "\nYou are running version " <> SemVer.toText pscPackageVersion

getPscPackageVersion :: MonadIO m => m SemVer.Version
getPscPackageVersion = liftIO $ parseSemVer =<< runPscPackageCmd "--version"
  where
  parseSemVer t = either rethrow return (SemVer.fromText t)
    where
    rethrow err = die $
      "Error parsing version from psc-package: " <> Text.pack err

getPscPackageSourcePaths :: MonadIO m => m (Set FilePath)
getPscPackageSourcePaths = liftIO $ Set.fromList <$>
    (globAll . Set.toList =<< getPscPackageSources)
  where
  globAll :: [Text] -> IO [FilePath]
  globAll globs = fmap (Path.fromText . Text.pack) . concat
    <$> traverse (glob . Text.unpack) globs

getPscPackageSources :: MonadIO m => m (Set Text)
getPscPackageSources = Set.fromList . Text.lines <$> runPscPackageCmd "sources"

runPscPackageCmd :: MonadIO m => Text -> m Text
runPscPackageCmd cmd = liftIO $ Text.stripEnd <$> proc' "psc-package" [cmd]
  where
  proc' cmd args = do
    (exitCode, out) <- procStrict cmd args empty
    case exitCode of
      ExitSuccess -> return out
      _           -> throw (ProcFailed cmd args exitCode)

getModuleNames :: MonadIO m => Set FilePath -> m (Set Text)
getModuleNames paths = do
  (errors, mNames) <- partitionEithers <$> liftIO getModuleNamesAndErrors
  traverse_ stderrException errors
  return $ Set.fromList mNames
  where
  getModuleNamesAndErrors = traverse (tryAny . getModuleName) (Set.toList paths)

getModuleName :: MonadIO m => FilePath -> m Text
getModuleName path = liftIO $
  fold (input path) (Fold.foldMap (First . parseModuleName) getFirst)
  >>= maybe (die errText) return
  where
  parseModuleName :: Line -> Maybe Text
  parseModuleName = lineToText >>> match pat >>> headMay
    where
    pat = "module "
          *> prefix (plus $ satisfy (not . ((||) <$> isSpace <*> ('(' ==))))

  errText = "Couldn't parse module name from " <> pathToText path

genDocs :: [FilePath] -> [Text] -> IO ()
genDocs paths moduleNames = procs "purs" args empty
  where
  args = "docs" : (pathToText <$> paths)
         ++ docgen : List.intersperse docgen docargs
    where
    docgen = "--docgen"
    docargs = flip map moduleNames $ \mName -> mName <> ":"
      <> pathToText ("generated-docs" </> Path.fromText mName <.> "md")

pathToText :: FilePath -> Text
pathToText = either id id . Path.toText

stderrException :: (MonadIO m, Exception e) => e -> m ()
stderrException = stderr . select . textToLines' . Text.pack . displayException
  where textToLines' = NEList.toList . textToLines
