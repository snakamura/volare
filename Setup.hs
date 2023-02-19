module Main (main) where

import Control.Monad
    ( unless
    , when
    )
import Data.Maybe
    ( fromJust
    , fromMaybe
    )
import qualified Distribution.PackageDescription as PD
import Distribution.Simple
    ( Args
    , UserHooks
    , buildHook
    , confHook
    , defaultMainWithHooks
    , postClean
    , postConf
    , preConf
    , simpleUserHooks
    )
import Distribution.Simple.LocalBuildInfo
    ( LocalBuildInfo
    , configFlags
    , localPkgDescr
    )
import Distribution.Simple.Setup
    ( BuildFlags
    , CleanFlags
    , ConfigFlags
    , buildVerbosity
    , cleanVerbosity
    , configConfigurationsFlags
    , configVerbosity
    , fromFlag
    )
import Distribution.Simple.Utils (rawSystemExit)
import System.Directory
    ( doesDirectoryExist
    , getCurrentDirectory
    , removeDirectoryRecursive
    )


main :: IO ()
main = defaultMainWithHooks simpleUserHooks {
           preConf = volarePreConf,
           confHook = volareConfHook,
           postConf = volarePostConf,
           buildHook = volareBuildHook,
           postClean = volarePostClean
       }


volarePreConf :: Args ->
                 ConfigFlags ->
                 IO PD.HookedBuildInfo
volarePreConf args flags = do
    buildInfo <- preConf simpleUserHooks args flags
    let glibcxx = fromMaybe False $ PD.lookupFlagAssignment (PD.mkFlagName "glibcxx") $ configConfigurationsFlags flags
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
                    PD.includeDirs = (dir ++ "/msm"):PD.includeDirs libraryBuildInfo,
                    PD.extraLibDirs = (dir ++ "/msm/lib"):PD.extraLibDirs libraryBuildInfo
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
    rawSystemExit (fromFlag $ configVerbosity flags) "npx" ["grunt", "bower:install"]
    postConf simpleUserHooks args flags description localBuildInfo


volareBuildHook :: PD.PackageDescription ->
                   LocalBuildInfo ->
                   UserHooks ->
                   BuildFlags ->
                   IO ()
volareBuildHook description localBuildInfo hooks flags = do
    let f = configConfigurationsFlags $ configFlags localBuildInfo
        dev = fromMaybe False (PD.lookupFlagAssignment (PD.mkFlagName "dev") f) || fromMaybe False (PD.lookupFlagAssignment (PD.mkFlagName "library-only") f)
    unless dev $
        rawSystemExit (fromFlag $ buildVerbosity flags) "npx" ["grunt", "build"]
    buildHook simpleUserHooks description localBuildInfo hooks flags


volarePostClean :: Args ->
                   CleanFlags ->
                   PD.PackageDescription ->
                   () ->
                   IO ()
volarePostClean args flags description _ = do
    rawSystemExit (fromFlag $ cleanVerbosity flags) "/bin/sh" ["-c", "cd msm && make clean"]
    b <- doesDirectoryExist "node_modules"
    when b $
        rawSystemExit (fromFlag $ cleanVerbosity flags) "npx" ["grunt", "clean"]
    safeRemoveDirectory "node_modules"
    postClean simpleUserHooks args flags description ()


safeRemoveDirectory :: FilePath ->
                       IO ()
safeRemoveDirectory path = do
    b <- doesDirectoryExist path
    when b $
        removeDirectoryRecursive path
