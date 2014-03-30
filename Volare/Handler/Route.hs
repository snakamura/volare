module Volare.Handler.Route (
    postRoutesR,
    getRouteR,
    deleteRouteR
) where

import Control.Applicative ((<$>),
                            (<*>))
import Control.Arrow (second)
import Data.Aeson ((.=),
                   (.:))
import qualified Data.Aeson as JSON
import Data.Foldable (forM_)
import Data.Maybe (fromJust,
                   isJust)
import Data.Monoid (mempty)
import Data.Traversable (forM)
import Database.Persist (Entity(Entity),
                         PersistEntityBackend,
                         PersistMonadBackend,
                         PersistStore,
                         SelectOpt(Asc),
                         (==.),
                         deleteCascade,
                         insert,
                         selectFirst,
                         selectList)
import Yesod.Core.Json (requireJsonBody)
import Yesod.Persist (get404,
                      runDB)

import Volare.Foundation
import qualified Volare.Model as M


data NewRoute = NewRoute [NewRouteItem]

instance JSON.FromJSON NewRoute where
    parseJSON a@(JSON.Array _) = NewRoute <$> JSON.parseJSON a
    parseJSON _ = mempty


data NewRouteItem = NewRouteItem M.WaypointItemId Int

instance JSON.FromJSON NewRouteItem where
    parseJSON (JSON.Object o) = NewRouteItem <$> o .: "waypointItemId"
                                             <*> o .: "radius"
    parseJSON _ = mempty


postRoutesR :: Handler JSON.Value
postRoutesR = do
    NewRoute items <- requireJsonBody
    route <- runDB $ do
        routeId <- addRoute items
        selectFirst [M.RouteId ==. routeId] []
    return $ JSON.toJSON route


data Route = Route M.RouteId [RouteItem]

instance JSON.ToJSON Route where
    toJSON (Route routeId items) =
        JSON.object [
            "id" .= routeId,
            "items" .= items
          ]


data RouteItem = RouteItem M.RouteItemId (Entity M.WaypointItem) Int

instance JSON.ToJSON RouteItem where
    toJSON (RouteItem routeItemId waypointItem radius) =
        JSON.object [
            "id" .= routeItemId,
            "waypointItem" .= waypointItem,
            "radius" .= radius
          ]


getRouteR :: M.RouteId ->
             Handler JSON.Value
getRouteR routeId = do
    items <- runDB $ do
        _ <- get404 routeId
        items <- selectList [M.RouteItemRouteId ==. routeId] [Asc M.RouteItemIndex]
        waypointItems <- forM items $ \(Entity _ item) ->
            selectFirst [M.WaypointItemId ==. M.routeItemWaypointItemId item] []
        return $ map (second fromJust) $ filter (isJust . snd) $ zip items waypointItems
    return $ JSON.toJSON $ Route routeId $ map (uncurry makeRouteItem) items
    where
      makeRouteItem (Entity itemId item) waypointItem = RouteItem itemId waypointItem (M.routeItemRadius item)


deleteRouteR :: M.RouteId ->
                Handler JSON.Value
deleteRouteR routeId = do
    runDB $ deleteCascade routeId
    return $ JSON.toJSON ()


addRoute :: (PersistStore m, PersistMonadBackend m ~ PersistEntityBackend M.Route) =>
            [NewRouteItem] ->
            m M.RouteId
addRoute items = do
    routeId <- insert $ M.Route
    forM_ (zip [0..] items) $ \(index, NewRouteItem waypointItemId radius) ->
        insert $ M.RouteItem routeId index waypointItemId radius
    return routeId
