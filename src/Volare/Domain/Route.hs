module Volare.Domain.Route
    ( Route(Route)
    , RouteItem(RouteItem)
    , getRoute
    , getRouteWithWaypoints
    , addRoute
    , deleteRoute
    ) where

import Control.Arrow (second)
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

import Volare.Domain.Types
    ( Delete
    , Query
    , Store)
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


getRoute :: P.Key M.Route ->
            Query M.Route (Maybe (P.Entity M.Route))
getRoute routeId = P.selectFirst [M.RouteId ==. routeId] []


getRouteWithWaypoints :: P.Key M.Route ->
                         Query M.Route (Maybe Route)
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


addRoute :: [(M.WaypointItemId, Int)] ->
            Store M.Route (P.Key M.Route)
addRoute items = do
    routeId <- P.insert M.Route
    forM_ (zip [0..] items) $ \(index, (waypointItemId, radius)) ->
        P.insert $ M.RouteItem routeId index waypointItemId radius
    return routeId


deleteRoute :: P.Key M.Route ->
               Delete M.Route ()
deleteRoute = P.deleteCascade
