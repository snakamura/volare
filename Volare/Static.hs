{-# LANGUAGE OverloadedStrings,
             TemplateHaskell #-}

module Volare.Static where

import Yesod.Static (Route(..),
                     Static,
                     StaticRoute,
                     static,
                     staticFiles)

import Volare.Static.Dir (staticDir)


staticSite :: IO Static
staticSite = static staticDir


staticFiles staticDir
