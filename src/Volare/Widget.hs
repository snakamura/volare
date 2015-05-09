module Volare.Widget
    ( Page(..)
    , navigation
    ) where

import qualified Data.Text as T

import Volare.Foundation
import Volare.Settings (widgetFile)


data Page = FLIGHTS
          | WORKSPACES
          | WAYPOINTS
          | UAS
          | FORCAST
    deriving (Show, Eq)


navigation :: Page ->
              Widget
navigation page = $(widgetFile "widgets/navigation")
  where
    classes p | p == page = "active" :: T.Text
              | otherwise = ""
