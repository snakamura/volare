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

import qualified Volare.Model as M


getWaypoints :: (P.PersistQuery m, P.PersistMonadBackend m ~ P.PersistEntityBackend M.Waypoint) =>
                m [P.Entity M.Waypoint]
getWaypoints = P.selectList [] [P.Asc M.WaypointName]


getWaypoint :: (P.PersistQuery m, P.PersistMonadBackend m ~ P.PersistEntityBackend M.Waypoint) =>
               M.WaypointId ->
               m (Maybe (P.Entity M.Waypoint))
getWaypoint waypointId = P.selectFirst [M.WaypointId ==. waypointId] []


getWaypointItems :: (P.PersistQuery m, P.PersistMonadBackend m ~ P.PersistEntityBackend M.Waypoint) =>
                    M.WaypointId ->
                    m [P.Entity M.WaypointItem]
getWaypointItems waypointId = P.selectList [M.WaypointItemWaypointId ==. waypointId] [P.Asc M.WaypointItemName]


addWaypoint :: (P.PersistStore m, P.PersistMonadBackend m ~ P.PersistEntityBackend M.Waypoint) =>
               T.Text ->
               GeoWpt.Wpt ->
               m M.WaypointId
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


updateWaypoint :: (P.PersistQuery m, P.PersistMonadBackend m ~ P.PersistEntityBackend M.Waypoint) =>
                  M.WaypointId ->
                  Maybe T.Text ->
                  m ()
updateWaypoint waypointId name =
    forM_ name $ \newName ->
        P.update waypointId [M.WaypointName =. newName]


deleteWaypoint :: (P.PersistQuery m, P.PersistMonadBackend m ~ P.PersistEntityBackend M.Waypoint) =>
                  M.WaypointId ->
                  m ()
deleteWaypoint = P.deleteCascade
