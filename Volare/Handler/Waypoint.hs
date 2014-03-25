module Volare.Handler.Waypoint (
    getWaypointsR,
    postWaypointsR,
    getWaypointR,
    putWaypointR,
    deleteWaypointR
) where

import qualified Codec.GeoWpt as GeoWpt
import Control.Applicative ((<$>),
                            (<*>))
import Data.Aeson ((.=),
                   (.:))
import qualified Data.Aeson as JSON
import Data.Attoparsec (parseOnly)
import qualified Data.ByteString as B
import Data.Foldable (forM_)
import Data.Monoid ((<>),
                    mempty)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Database.Persist (Entity,
                         PersistEntityBackend,
                         PersistMonadBackend,
                         PersistStore,
                         SelectOpt(Asc),
                         (=.),
                         (==.),
                         deleteCascade,
                         insert,
                         selectFirst,
                         selectList,
                         update)
import Text.Blaze.Html (toHtml)
import Yesod.Core (defaultLayout)
import Yesod.Core.Handler (invalidArgs,
                           provideRep,
                           selectRep)
import Yesod.Core.Json (requireJsonBody)
import Yesod.Core.Types (TypedContent)
import Yesod.Core.Widget (addScript,
                          addStylesheet,
                          setTitle)
import Yesod.Persist (get404,
                      runDB)

import Volare.Foundation
import Volare.Handler.Utils (addCommonLibraries,
                             addGoogleMapsApi)
import qualified Volare.Model as M
import Volare.Settings (widgetFile)
import qualified Volare.Static as S


getWaypointsR :: Handler TypedContent
getWaypointsR = do
    selectRep $ do
        provideRep $ defaultLayout $ do
            setTitle "Waypoints - Volare"
            addCommonLibraries
            addScript $ StaticR S.js_common_js
            addScript $ StaticR S.js_waypoints_js
            addStylesheet $ StaticR S.css_common_css
            $(widgetFile "waypoints/index")
        provideRep $ runDB $ JSON.toJSON <$> selectList [] [Asc M.WaypointName]


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


data Waypoint = Waypoint M.WaypointId M.Waypoint [Entity M.WaypointItem]

instance JSON.ToJSON Waypoint where
    toJSON (Waypoint waypointId waypoint items) =
        JSON.object [
            "id" .= waypointId,
            "name" .= M.waypointName waypoint,
            "items" .= items
          ]


getWaypointR :: M.WaypointId ->
                Handler TypedContent
getWaypointR waypointId = do
    waypoint <- runDB $ get404 waypointId
    selectRep $ do
        provideRep $ defaultLayout $ do
            setTitle $ toHtml $ M.waypointName waypoint <> " - Waypoints - Volare"
            addCommonLibraries
            addGoogleMapsApi
            addScript $ StaticR S.js_common_js
            addScript $ StaticR S.js_waypoint_js
            addStylesheet $ StaticR S.css_common_css
            addStylesheet $ StaticR S.css_waypoint_css
            $(widgetFile "waypoints/show")
        provideRep $ do
            items <- runDB $ selectList [M.WaypointItemWaypointId ==. waypointId] [Asc M.WaypointItemName]
            return $ JSON.toJSON $ Waypoint waypointId waypoint items


data EditWaypoint = EditWaypoint T.Text

instance JSON.FromJSON EditWaypoint where
    parseJSON (JSON.Object o) = EditWaypoint <$> o .: "name"
    parseJSON _ = mempty


putWaypointR :: M.WaypointId ->
                Handler JSON.Value
putWaypointR waypointId = do
    EditWaypoint name <- requireJsonBody
    waypoint <- runDB $ do
        update waypointId [M.WaypointName =. name]
        selectFirst [M.WaypointId ==. waypointId] []
    return $ JSON.toJSON waypoint


deleteWaypointR :: M.WaypointId ->
                   Handler JSON.Value
deleteWaypointR waypointId = do
    runDB $ deleteCascade waypointId
    return $ JSON.toJSON ()


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
