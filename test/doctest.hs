module Main (main) where

import Test.DocTest (doctest)


main :: IO ()
main = doctest [ "-isrc"
               , "-XOverloadedStrings"
               , "-XRankNTypes"
               , "-XScopedTypeVariables"
               , "src/Codec/IGC/Utils.hs"
               , "src/Codec/Utils/Pipes.hs"
               , "src/Service/UAS/Parser.hs"
               , "src/Service/WINDAS/Parser.hs"
               ]
