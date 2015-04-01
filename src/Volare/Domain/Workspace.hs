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

import Control.Monad
    ( join
    , when
    )
import Control.Monad.IO.Class (liftIO)
import Data.Aeson ((.=))
import qualified Data.Aeson as JSON
import Data.Bifunctor (first)
import Data.Foldable (forM_)
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
import Volare.Domain.Types
    ( Query
    , Store)
import qualified Volare.Model as M


getWorkspaces :: Query M.Workspace [P.Entity M.Workspace]
getWorkspaces = P.selectList [] [P.Desc M.WorkspaceCreated]


getWorkspace :: P.Key M.Workspace ->
                Query M.Workspace (Maybe (P.Entity M.Workspace))
getWorkspace workspaceId = P.selectFirst [M.WorkspaceId ==. workspaceId] []


addWorkspace :: T.Text ->
                Store M.Workspace (P.Key M.Workspace)
addWorkspace name = do
    created <- liftIO getCurrentTime
    P.insert $ M.Workspace name created Nothing


updateWorkspace :: P.Key M.Workspace ->
                   Maybe T.Text ->
                   Maybe (Maybe M.RouteId) ->
                   Store M.Workspace ()
updateWorkspace workspaceId name routeId = do
    forM_ name $ \newName ->
        P.update workspaceId [M.WorkspaceName =. newName]
    forM_ routeId $ \newRouteId -> do
        workspace <- getWorkspace workspaceId
        P.update workspaceId [M.WorkspaceRoute =. newRouteId]
        forM_ (P.entityVal <$> workspace >>= M.workspaceRoute) $ \oldRouteId -> do
            when (Just oldRouteId /= newRouteId) $
                DR.deleteRoute oldRouteId


deleteWorkspace :: P.Key M.Workspace ->
                   Query M.Workspace ()
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


getWorkspaceFlights :: P.Key M.Workspace ->
                       Query M.WorkspaceFlight [WorkspaceFlight]
getWorkspaceFlights workspaceId = do
    workspaceFlights <- P.selectList [M.WorkspaceFlightWorkspaceId ==. workspaceId] []
    (sortBy (comparing name) . catMaybes) <$> mapM getWorkspaceFlight' workspaceFlights
  where
    name (WorkspaceFlight _ flight _) = M.flightName flight


getWorkspaceFlight :: P.Key M.WorkspaceFlight ->
                      Query M.WorkspaceFlight (Maybe WorkspaceFlight)
getWorkspaceFlight workspaceFlightId = do
    workspaceFlight <- P.selectFirst [M.WorkspaceFlightId ==. workspaceFlightId] []
    join <$> mapM getWorkspaceFlight' workspaceFlight


getWorkspaceFlight' :: P.Entity M.WorkspaceFlight ->
                       Query M.WorkspaceFlight (Maybe WorkspaceFlight)
getWorkspaceFlight' workspaceFlight = fmap makeWorkspaceFlight <$> getFlight workspaceFlight
  where
    getFlight (P.Entity _ (M.WorkspaceFlight _ flightId color)) = fmap (, color) <$> DF.getFlight flightId
    makeWorkspaceFlight (P.Entity flightId flight, color) = WorkspaceFlight flightId flight color


getWorkspaceCandidateFlights :: P.Key M.Workspace ->
                                Query M.Workspace [P.Entity M.Flight]
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


addWorkspaceFlight :: P.Key M.Workspace ->
                      [P.Key M.Flight] ->
                      Store M.WorkspaceFlight [WorkspaceFlight]
addWorkspaceFlight workspaceId flightIds = do
    flights <- forM flightIds $ \flightId -> do
        color <- nextColor workspaceId
        insertedFlightId <- P.insertUnique $ M.WorkspaceFlight workspaceId flightId color
        join <$> mapM getWorkspaceFlight insertedFlightId
    return $ catMaybes flights


deleteWorkspaceFlight :: P.Key M.Workspace ->
                         P.Key M.Flight ->
                         Query M.WorkspaceFlight ()
deleteWorkspaceFlight workspaceId flightId =
    P.deleteWhere [M.WorkspaceFlightWorkspaceId ==. workspaceId,
                   M.WorkspaceFlightFlightId ==. flightId]


nextColor :: P.Key M.Workspace ->
             Query M.Workspace T.Text
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
