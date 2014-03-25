module Volare.Handler.Waypoint (
    getWaypointsR,
    postWaypointsR,
    getWaypointR,
    deleteWaypointR
) where

import qualified Data.Aeson as JSON
import Database.Persist (Entity(Entity),
                         selectList)
import Text.Blaze.Html (Html)
import Yesod.Core (defaultLayout)
import Yesod.Core.Widget (addScript,
                          addStylesheet,
                          setTitle)
import Yesod.Persist (runDB)

import Volare.Foundation
import Volare.Handler.Utils (addCommonLibraries)
import qualified Volare.Model as M
import Volare.Settings (widgetFile)
import qualified Volare.Static as S


getWaypointsR :: Handler Html
getWaypointsR = do
    waypoints <- runDB $ selectList [] []
    defaultLayout $ do
        setTitle "Waypoints - Volare"
        addCommonLibraries
        addScript $ StaticR S.js_common_js
        addStylesheet $ StaticR S.css_common_css
        $(widgetFile "waypoints/index")


postWaypointsR :: Handler JSON.Value
postWaypointsR = undefined


getWaypointR :: M.WaypointId ->
                Handler Html
getWaypointR = undefined


deleteWaypointR :: M.WaypointId ->
                   Handler JSON.Value
deleteWaypointR = undefined
