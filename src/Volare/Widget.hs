module Volare.Widget
    ( Page(..)
    , navigation
    , waypoint
    , route
    ) where

import qualified Data.Text as T

import Volare.Foundation
import Volare.Settings (widgetFile)


data Page = FLIGHTS
          | WORKSPACES
          | WAYPOINTS
    deriving (Show, Eq)


navigation :: Page ->
              Widget
navigation page = $(widgetFile "widgets/navigation")
  where
    classes p | p == page = "active" :: T.Text
              | otherwise = ""


waypoint :: Widget
waypoint = $(widgetFile "widgets/waypoint")


route :: Widget
route = $(widgetFile "widgets/route")
