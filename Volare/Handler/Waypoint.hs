module Volare.Handler.Waypoint (
    getWaypointsR,
    postWaypointsR,
    getWaypointR,
    deleteWaypointR
) where

import qualified Codec.GeoWpt as GeoWpt
import Control.Applicative ((<$>),
                            (<*>))
import Data.Aeson ((.:))
import qualified Data.Aeson as JSON
import Data.Attoparsec (parseOnly)
import qualified Data.ByteString as B
import Data.Foldable (forM_)
import Data.Monoid (mempty)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Database.Persist (Entity(Entity),
                         PersistEntityBackend,
                         PersistMonadBackend,
                         PersistStore,
                         SelectOpt(Asc),
                         (==.),
                         insert,
                         selectFirst,
                         selectList)
import Text.Blaze.Html (Html,
                        toHtml)
import Yesod.Core (defaultLayout)
import Yesod.Core.Handler (invalidArgs)
import Yesod.Core.Json (requireJsonBody)
import Yesod.Core.Widget (addScript,
                          addStylesheet,
                          setTitle)
import Yesod.Persist (get404,
                      runDB)

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
        addScript $ StaticR S.js_waypoints_js
        addStylesheet $ StaticR S.css_common_css
        $(widgetFile "waypoints/index")


data NewWaypoint = NewWaypoint T.Text B.ByteString

instance JSON.FromJSON NewWaypoint where
    parseJSON (JSON.Object o) = NewWaypoint <$> o .: "name"
                                            <*> (T.encodeUtf8 <$> (o .: "wpt"))
    parseJSON _ = mempty


postWaypointsR :: Handler JSON.Value
postWaypointsR = do
    NewWaypoint name wptBytes <- requireJsonBody
    case parseOnly GeoWpt.wpt wptBytes of
      Left _ -> invalidArgs ["wpt"]
      Right wpt -> do
          waypoint <- runDB $ do
              waypointId <- addWaypoint name wpt
              selectFirst [M.WaypointId ==. waypointId] []
          return $ JSON.toJSON waypoint


getWaypointR :: M.WaypointId ->
                Handler Html
getWaypointR waypointId = do
    waypoint <- runDB $ get404 waypointId
    items <- runDB $ selectList [M.WaypointItemWaypointId ==. waypointId] [Asc M.WaypointItemName]
    defaultLayout $ do
        setTitle $ toHtml $ M.waypointName waypoint `T.append` " - Waypoints - Volare"
        addCommonLibraries
        addScript $ StaticR S.js_common_js
        addStylesheet $ StaticR S.css_common_css
        $(widgetFile "waypoints/show")


deleteWaypointR :: M.WaypointId ->
                   Handler JSON.Value
deleteWaypointR = undefined


addWaypoint :: (PersistStore m, PersistMonadBackend m ~ PersistEntityBackend M.Waypoint) =>
               T.Text ->
               GeoWpt.Wpt ->
               m M.WaypointId
addWaypoint name wpt = do
    waypointId <- insert $ M.Waypoint name
    forM_ (GeoWpt.items wpt) $ \item ->
        insert $ M.WaypointItem waypointId
                                (GeoWpt.name item)
                                (realToFrac $ GeoWpt.latitude item)
                                (realToFrac $ GeoWpt.longitude item)
                                (realToFrac $ GeoWpt.altitude item)
                                (GeoWpt.description item)
    return waypointId
