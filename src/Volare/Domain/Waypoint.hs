module Volare.Domain.Waypoint
    ( getWaypoints
    , getWaypoint
    , getWaypointItems
    , addWaypoint
    , updateWaypoint
    , deleteWaypoint
) where

import qualified Codec.GeoWpt as GeoWpt
import Data.Foldable (forM_)
import qualified Data.Text as T
import Database.Persist
    ( (=.)
    , (==.)
    )
import qualified Database.Persist as P

import Volare.Domain.Types
    ( Delete
    , Query
    , Store)
import qualified Volare.Model as M


getWaypoints :: Query M.Waypoint [P.Entity M.Waypoint]
getWaypoints = P.selectList [] [P.Asc M.WaypointName]


getWaypoint :: P.Key M.Waypoint ->
               Query M.Waypoint (Maybe (P.Entity M.Waypoint))
getWaypoint waypointId = P.selectFirst [M.WaypointId ==. waypointId] []


getWaypointItems :: P.Key M.Waypoint ->
                    Query M.Waypoint [P.Entity M.WaypointItem]
getWaypointItems waypointId = P.selectList [M.WaypointItemWaypointId ==. waypointId] [P.Asc M.WaypointItemName]


addWaypoint :: T.Text ->
               GeoWpt.Wpt ->
               Store M.Waypoint (P.Key M.Waypoint)
addWaypoint name wpt = do
    waypointId <- P.insert $ M.Waypoint name
    forM_ (GeoWpt.items wpt) $ \item ->
        P.insert $ M.WaypointItem waypointId
                                  (GeoWpt.name item)
                                  (realToFrac $ GeoWpt.latitude item)
                                  (realToFrac $ GeoWpt.longitude item)
                                  (realToFrac $ GeoWpt.altitude item)
                                  (GeoWpt.description item)
    return waypointId


updateWaypoint :: P.Key M.Waypoint ->
                  Maybe T.Text ->
                  Store M.Waypoint ()
updateWaypoint waypointId name =
    forM_ name $ \newName ->
        P.update waypointId [M.WaypointName =. newName]


deleteWaypoint :: P.Key M.Waypoint ->
                  Delete M.Waypoint ()
deleteWaypoint = P.delete
