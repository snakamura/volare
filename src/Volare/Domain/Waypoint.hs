module Volare.Domain.Waypoint
    ( getWaypoints
    , getWaypoint
    , getWaypointItems
    , addWaypoint
    , updateWaypoint
    , deleteWaypoint
) where

import qualified Codec.GeoWpt as GeoWpt
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Foldable (forM_)
import qualified Data.Text as T
import Database.Persist
    ( (=.)
    , (==.)
    )
import qualified Database.Persist as P

import qualified Volare.Model as M


getWaypoints :: (MonadIO m, P.PersistQuery backend, backend ~ P.PersistEntityBackend M.Waypoint) =>
                ReaderT backend m [P.Entity M.Waypoint]
getWaypoints = P.selectList [] [P.Asc M.WaypointName]


getWaypoint :: (MonadIO m, P.PersistQuery backend, backend ~ P.PersistEntityBackend M.Waypoint) =>
               P.Key M.Waypoint ->
               ReaderT backend m (Maybe (P.Entity M.Waypoint))
getWaypoint waypointId = P.selectFirst [M.WaypointId ==. waypointId] []


getWaypointItems :: (MonadIO m, P.PersistQuery backend, backend ~ P.PersistEntityBackend M.Waypoint) =>
                    P.Key M.Waypoint ->
                    ReaderT backend m [P.Entity M.WaypointItem]
getWaypointItems waypointId = P.selectList [M.WaypointItemWaypointId ==. waypointId] [P.Asc M.WaypointItemName]


addWaypoint :: (MonadIO m, P.PersistStore backend, backend ~ P.PersistEntityBackend M.Waypoint) =>
               T.Text ->
               GeoWpt.Wpt ->
               ReaderT backend m (P.Key M.Waypoint)
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


updateWaypoint :: (MonadIO m, P.PersistStore backend, backend ~ P.PersistEntityBackend M.Waypoint) =>
                  P.Key M.Waypoint ->
                  Maybe T.Text ->
                  ReaderT backend m ()
updateWaypoint waypointId name =
    forM_ name $ \newName ->
        P.update waypointId [M.WaypointName =. newName]


deleteWaypoint :: (MonadIO m, P.PersistStore backend, P.DeleteCascade M.Waypoint backend) =>
                  P.Key M.Waypoint ->
                  ReaderT backend m ()
deleteWaypoint = P.deleteCascade
