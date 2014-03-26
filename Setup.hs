module Main (main) where

import Data.Maybe (fromJust)
import qualified Distribution.PackageDescription as PD
import Distribution.Simple (confHook,
                            defaultMainWithHooks,
                            simpleUserHooks)
import Distribution.Simple.LocalBuildInfo (localPkgDescr)
import System.Directory (getCurrentDirectory)


main :: IO ()
main = defaultMainWithHooks simpleUserHooks { confHook = volareConfHook }


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
