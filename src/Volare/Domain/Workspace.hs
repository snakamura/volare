module Volare.Domain.Workspace
    ( WorkspaceFlight(WorkspaceFlight)
    , getWorkspaces
    , getWorkspace
    , addWorkspace
    , updateWorkspace
    , deleteWorkspace
    , getWorkspaceFlights
    , getWorkspaceCandidateFlights
    , addWorkspaceFlight
    , deleteWorkspaceFlight
    ) where

import Control.Arrow (first)
import Control.Monad
    ( join
    , when
    )
import Control.Monad.IO.Class
    ( MonadIO
    , liftIO
    )
import Control.Monad.Trans.Reader (ReaderT)
import Data.Aeson ((.=))
import qualified Data.Aeson as JSON
import Data.Foldable (forM_)
import Data.Functor ((<$>))
import Data.List
    ( minimumBy
    , sortBy
    )
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import qualified Data.Text as T
import Data.Time
    ( addUTCTime
    , getCurrentTime
    )
import Data.Traversable
    ( forM
    , mapM
    )
import Database.Persist
    ( (=.)
    , (==.)
    , (<=.)
    , (>=.)
    )
import qualified Database.Persist as P
import Prelude hiding (mapM)

import qualified Volare.Domain.Flight as DF
import qualified Volare.Domain.Route as DR
import qualified Volare.Model as M

getWorkspaces :: (MonadIO m, backend ~ P.PersistEntityBackend M.Workspace) =>
                 ReaderT backend m [P.Entity M.Workspace]
getWorkspaces = P.selectList [] [P.Desc M.WorkspaceCreated]


getWorkspace :: (MonadIO m, backend ~ P.PersistEntityBackend M.Workspace) =>
                P.Key M.Workspace ->
                ReaderT backend m (Maybe (P.Entity M.Workspace))
getWorkspace workspaceId = P.selectFirst [M.WorkspaceId ==. workspaceId] []


addWorkspace :: (MonadIO m, backend ~ P.PersistEntityBackend M.Workspace) =>
                T.Text ->
                ReaderT backend m (P.Key M.Workspace)
addWorkspace name = do
    created <- liftIO getCurrentTime
    P.insert $ M.Workspace name created Nothing


updateWorkspace :: (MonadIO m, backend ~ P.PersistEntityBackend M.Workspace) =>
                   P.Key M.Workspace ->
                   Maybe T.Text ->
                   Maybe (Maybe M.RouteId) ->
                   ReaderT backend m ()
updateWorkspace workspaceId name routeId = do
    forM_ name $ \newName ->
        P.update workspaceId [M.WorkspaceName =. newName]
    forM_ routeId $ \newRouteId -> do
        workspace <- getWorkspace workspaceId
        P.update workspaceId [M.WorkspaceRoute =. newRouteId]
        forM_ (P.entityVal <$> workspace >>= M.workspaceRoute) $ \oldRouteId -> do
            when (Just oldRouteId /= newRouteId) $
                DR.deleteRoute oldRouteId


deleteWorkspace :: (MonadIO m, backend ~ P.PersistEntityBackend M.Workspace) =>
                   P.Key M.Workspace ->
                   ReaderT backend m ()
deleteWorkspace workspaceId = do
    workspace <- getWorkspace workspaceId
    P.deleteWhere [M.WorkspaceFlightWorkspaceId ==. workspaceId]
    forM_ (P.entityVal <$> workspace >>= M.workspaceRoute) DR.deleteRoute
    P.delete workspaceId


data WorkspaceFlight = WorkspaceFlight M.FlightId M.Flight T.Text

instance JSON.ToJSON WorkspaceFlight where
    toJSON (WorkspaceFlight workspaceId flight color) =
        JSON.object [ "id" .= workspaceId
                    , "name" .= M.flightName flight
                    , "color" .= color
                    ]


getWorkspaceFlights :: (Functor m, MonadIO m, backend ~ P.PersistEntityBackend M.Workspace) =>
                       P.Key M.Workspace ->
                       ReaderT backend m [WorkspaceFlight]
getWorkspaceFlights workspaceId = do
    workspaceFlights <- P.selectList [M.WorkspaceFlightWorkspaceId ==. workspaceId] []
    (sortBy (comparing name) . catMaybes) <$> mapM getWorkspaceFlight' workspaceFlights
  where
    name (WorkspaceFlight _ flight _) = M.flightName flight


getWorkspaceFlight :: (Functor m, MonadIO m, backend ~ P.PersistEntityBackend M.Workspace) =>
                      P.Key M.WorkspaceFlight ->
                      ReaderT backend m (Maybe WorkspaceFlight)
getWorkspaceFlight workspaceFlightId = do
    workspaceFlight <- P.selectFirst [M.WorkspaceFlightId ==. workspaceFlightId] []
    join <$> mapM getWorkspaceFlight' workspaceFlight


getWorkspaceFlight' :: (Functor m, MonadIO m, backend ~ P.PersistEntityBackend M.Workspace) =>
                       P.Entity M.WorkspaceFlight ->
                       ReaderT backend m (Maybe WorkspaceFlight)
getWorkspaceFlight' workspaceFlight = fmap makeWorkspaceFlight <$> getFlight workspaceFlight
  where
    getFlight (P.Entity _ (M.WorkspaceFlight _ flightId color)) = fmap (, color) <$> DF.getFlight flightId
    makeWorkspaceFlight (P.Entity flightId flight, color) = WorkspaceFlight flightId flight color


getWorkspaceCandidateFlights :: (Functor m, MonadIO m, backend ~ P.PersistEntityBackend M.Workspace) =>
                                P.Key M.Workspace ->
                                ReaderT backend m [P.Entity M.Flight]
getWorkspaceCandidateFlights workspaceId = do
    workspaceFlights <- getWorkspaceFlights workspaceId
    case workspaceFlights of
        [] -> P.selectList [] [P.Desc M.FlightTime, P.Asc M.FlightName]
        _ -> let times = map (\(WorkspaceFlight _ flight _) -> M.flightTime flight) workspaceFlights
                 start = addUTCTime (-6*60*60) $ minimum times
                 end = addUTCTime (6*60*60) $ maximum times
                 flightIds = map (\(WorkspaceFlight flightId _ _) -> flightId) workspaceFlights
                 included (P.Entity flightId _) = flightId `elem` flightIds
             in filter (not . included) <$> P.selectList [M.FlightTime >=. start, M.FlightTime <=. end] [P.Asc M.FlightName]


addWorkspaceFlight :: (Functor m, MonadIO m, backend ~ P.PersistEntityBackend M.Workspace) =>
                      P.Key M.Workspace ->
                      [P.Key M.Flight] ->
                      ReaderT backend m [WorkspaceFlight]
addWorkspaceFlight workspaceId flightIds = do
    flights <- forM flightIds $ \flightId -> do
        color <- nextColor workspaceId
        insertedFlightId <- P.insertUnique $ M.WorkspaceFlight workspaceId flightId color
        join <$> mapM getWorkspaceFlight insertedFlightId
    return $ catMaybes flights


deleteWorkspaceFlight :: (MonadIO m, backend ~ P.PersistEntityBackend M.Workspace) =>
                         P.Key M.Workspace ->
                         P.Key M.Flight ->
                         ReaderT backend m ()
deleteWorkspaceFlight workspaceId flightId =
    P.deleteWhere [M.WorkspaceFlightWorkspaceId ==. workspaceId,
                   M.WorkspaceFlightFlightId ==. flightId]


nextColor :: (Functor m, MonadIO m, backend ~ P.PersistEntityBackend M.Workspace) =>
             P.Key M.Workspace ->
             ReaderT backend m T.Text
nextColor workspaceId = do
    let colors = [ "red"
                 , "blue"
                 , "green"
                 , "yellow"
                 , "aqua"
                 , "fuchsia"
                 , "lime"
                 , "maroon"
                 , "navy"
                 , "olive"
                 , "purple"
                 , "silver"
                 , "teal"
                 ]
    usedColors <- map color <$> getWorkspaceFlights workspaceId
    let initialColorMap = Map.fromList $ zip colors $ zip (repeat (0 :: Int)) [0 :: Int ..]
        colorMap = foldr (Map.update (Just . first (+ 1))) initialColorMap usedColors
    return $ fst $ minimumBy (comparing snd) $ Map.assocs colorMap
  where
    color (WorkspaceFlight _ _ c) = c
