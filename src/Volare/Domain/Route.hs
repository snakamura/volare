module Volare.Domain.Route
    ( Route(Route)
    , RouteItem(RouteItem)
    , getRoute
    , getRouteWithWaypoints
    , addRoute
    , deleteRoute
    ) where

import Control.Arrow (second)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Aeson ((.=))
import qualified Data.Aeson as JSON
import Data.Maybe
    ( fromJust
    , isJust
    )
import Data.Foldable (forM_)
import Data.Traversable (forM)
import Database.Persist ((==.))
import qualified Database.Persist as P

import qualified Volare.Model as M


data Route = Route M.RouteId [RouteItem]

instance JSON.ToJSON Route where
    toJSON (Route routeId items) =
        JSON.object [ "id" .= routeId
                    , "items" .= items
                    ]


data RouteItem = RouteItem M.RouteItemId (P.Entity M.WaypointItem) Int

instance JSON.ToJSON RouteItem where
    toJSON (RouteItem routeItemId waypointItem radius) =
        JSON.object [ "id" .= routeItemId
                    , "waypointItem" .= waypointItem
                    , "radius" .= radius
                    ]


getRoute :: (MonadIO m, backend ~ P.PersistEntityBackend M.Route) =>
            P.Key M.Route ->
            ReaderT backend m (Maybe (P.Entity M.Route))
getRoute routeId = P.selectFirst [M.RouteId ==. routeId] []


getRouteWithWaypoints :: (MonadIO m, backend ~ P.PersistEntityBackend M.Route) =>
                         P.Key M.Route ->
                         ReaderT backend m (Maybe Route)
getRouteWithWaypoints routeId = do
    route <- P.selectFirst [M.RouteId ==. routeId] []
    case route of
        Just _ -> do
            routeItems <- P.selectList [M.RouteItemRouteId ==. routeId] [P.Asc M.RouteItemIndex]
            waypointItems <- forM routeItems $ \routeItemEntity ->
                P.selectFirst [M.WaypointItemId ==. M.routeItemWaypointItemId (P.entityVal routeItemEntity)] []
            return $ Just $ Route routeId $ map (uncurry makeRouteItem . second fromJust) $ filter (isJust . snd) $ zip routeItems waypointItems
        Nothing -> return Nothing
  where
    makeRouteItem routeItem waypointItem = RouteItem (P.entityKey routeItem) waypointItem (M.routeItemRadius $ P.entityVal routeItem)


addRoute :: (MonadIO m, backend ~ P.PersistEntityBackend M.Route) =>
            [(M.WaypointItemId, Int)] ->
            ReaderT backend m (P.Key M.Route)
addRoute items = do
    routeId <- P.insert M.Route
    forM_ (zip [0..] items) $ \(index, (waypointItemId, radius)) ->
        P.insert $ M.RouteItem routeId index waypointItemId radius
    return routeId


deleteRoute :: (MonadIO m, backend ~ P.PersistEntityBackend M.Route) =>
               P.Key M.Route ->
               ReaderT backend m ()
deleteRoute = P.deleteCascade
