module Main (main) where

import Network.Wai.Handler.Warp (run)

import Volare (withVolare)


main :: IO ()
main = withVolare (run 3000)
