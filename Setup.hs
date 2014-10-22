module Main (main) where

import Data.Maybe
    ( fromJust
    , fromMaybe
    )
import qualified Distribution.PackageDescription as PD
import Distribution.Simple
    ( Args
    , confHook
    , defaultMainWithHooks
    , postClean
    , postConf
    , preConf
    , simpleUserHooks
    )
import Distribution.Simple.LocalBuildInfo
    ( LocalBuildInfo
    , localPkgDescr
    )
import Distribution.Simple.Setup
    ( BuildFlags
    , CleanFlags
    , ConfigFlags
    , cleanVerbosity
    , configConfigurationsFlags
    , configVerbosity
    , fromFlag
    )
import Distribution.Simple.Utils (rawSystemExit)
import System.Directory (getCurrentDirectory)


main :: IO ()
main = defaultMainWithHooks simpleUserHooks {
           preConf = volarePreConf,
           confHook = volareConfHook,
           postConf = volarePostConf,
           postClean = volarePostClean
       }


volarePreConf :: Args ->
                 ConfigFlags ->
                 IO PD.HookedBuildInfo
volarePreConf args flags = do
    buildInfo <- preConf simpleUserHooks args flags
    let glibcxx = fromMaybe False $ lookup (PD.FlagName "glibcxx") $ configConfigurationsFlags flags
        macros = if glibcxx then
                     " GLIBCXX=1"
                 else
                     ""
    rawSystemExit (fromFlag $ configVerbosity flags) "/bin/sh" ["-c", "cd msm && make" ++ macros]
    return buildInfo


volareConfHook :: (PD.GenericPackageDescription, PD.HookedBuildInfo) ->
                  ConfigFlags ->
                  IO LocalBuildInfo
volareConfHook (description, buildInfo) flags = do
    localBuildInfo <- confHook simpleUserHooks (description, buildInfo) flags
    let packageDescription = localPkgDescr localBuildInfo
        library = fromJust $ PD.library packageDescription
        libraryBuildInfo = PD.libBuildInfo library
    dir <- getCurrentDirectory
    return localBuildInfo {
        localPkgDescr = packageDescription {
            PD.library = Just $ library {
                PD.libBuildInfo = libraryBuildInfo {
                    PD.extraLibDirs = (dir ++ "/msm"):PD.extraLibDirs libraryBuildInfo
                }
            }
        }
    }


volarePostConf :: Args ->
                  ConfigFlags ->
                  PD.PackageDescription ->
                  LocalBuildInfo ->
                  IO ()
volarePostConf args flags description localBuildInfo = do
    rawSystemExit (fromFlag $ configVerbosity flags) "npm" ["install"]
    rawSystemExit (fromFlag $ configVerbosity flags) "grunt" ["bower:install"]
    postConf simpleUserHooks args flags description localBuildInfo


volarePostClean :: Args ->
                   CleanFlags ->
                   PD.PackageDescription ->
                   () ->
                   IO ()
volarePostClean args flags description _ = do
    rawSystemExit (fromFlag $ cleanVerbosity flags) "/bin/sh" ["-c", "cd msm && make clean"]
    postClean simpleUserHooks args flags description ()
