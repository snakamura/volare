module Volare.Widget (
    Page(..),
    navigation,
    options,
    waypoint,
    route,
    weather
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


options :: Widget
options = $(widgetFile "widgets/options")


waypoint :: Widget
waypoint = $(widgetFile "widgets/waypoint")


route :: Widget
route = $(widgetFile "widgets/route")


weather :: Widget
weather = $(widgetFile "widgets/weather")
