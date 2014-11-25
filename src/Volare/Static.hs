module Volare.Static where

import Yesod.Static
    ( Static
    , static
    , staticDevel
    , staticFiles
    )

import Volare.Settings
    ( development
    , staticDir
    )


staticSite :: IO Static
staticSite = static' staticDir
  where
    static' = if development then
                  staticDevel
              else
                  static


staticFiles staticDir
