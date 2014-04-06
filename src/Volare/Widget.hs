module Volare.Widget (
    options,
    waypoint,
    route,
    weather
) where

import Volare.Foundation
import Volare.Settings (widgetFile)


options :: Widget
options = $(widgetFile "widgets/options")


waypoint :: Widget
waypoint = $(widgetFile "widgets/waypoint")


route :: Widget
route = $(widgetFile "widgets/route")


weather :: Widget
weather = $(widgetFile "widgets/weather")
