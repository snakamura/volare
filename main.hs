module Main (main) where

import Yesod.Default.Config (fromArgs)
import Yesod.Default.Main   (defaultMain)

import Volare (makeVolare)
import Volare.Config (parseConfig)


main :: IO ()
main = defaultMain (fromArgs $ const parseConfig) makeVolare
