{-# LANGUAGE FlexibleContexts,
             GADTs,
             MultiParamTypeClasses,
             OverloadedStrings,
             QuasiQuotes,
             TemplateHaskell,
             TypeFamilies #-}

module Main (main) where

import Control.Applicative ((<$>))
import qualified Data.Text as T
import Database.Persist (PersistEntity(..),
                         PersistEntityBackend,
                         PersistField(..))
import Database.Persist (Entity(Entity),
                         insert,
                         selectList)
import Database.Persist.GenericSql (ConnectionPool,
                                    SqlPersist,
                                    runMigration,
                                    runSqlPool)
import Database.Persist.Sqlite (withSqlitePool)
import Database.Persist.TH (mkMigrate,
                            mkPersist,
                            persist,
                            share,
                            sqlSettings)
import Text.Blaze.Html (Html)
import Text.Shakespeare.I18N (RenderMessage,
                              renderMessage)
import Yesod (warpDebug)
import Yesod.Core (Yesod,
                   defaultLayout,
                   renderRoute,
                   yesodDispatch)
import Yesod.Content (RepHtml)
import Yesod.Dispatch (mkYesod,
                       parseRoutes)
import Yesod.Form (FormMessage,
                   FormResult(FormSuccess),
                   MForm,
                   areq,
                   defaultFormMessage,
                   generateFormPost,
                   renderDivs,
                   runFormPost,
                   textField)
import Yesod.Handler (getYesod,
                      redirect)
import Yesod.Persist (YesodPersist(..),
                      get404)
import Yesod.Widget (whamletFile)

import Volare.Config (Config,
                      loadConfig,
                      sqlitePath,
                      sqliteConnectionPoolCount)


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persist|
Flight
  name T.Text
  deriving Show
|]


data Volare = Volare ConnectionPool


mkYesod "Volare" [parseRoutes|
/ RootR GET
/flights FlightsR GET POST
/flights/#FlightId FlightR GET
|]


instance Yesod Volare


instance YesodPersist Volare where
    type YesodPersistBackend Volare = SqlPersist

    runDB action = do
      Volare pool <- getYesod
      runSqlPool action pool


instance RenderMessage Volare FormMessage where
    renderMessage _ _ = defaultFormMessage


getRootR :: Handler RepHtml
getRootR = defaultLayout $(whamletFile "templates/root.hamlet")


flightForm :: Html ->
              MForm Volare Volare (FormResult Flight, Widget)
flightForm = renderDivs $ Flight <$> areq textField "Name" Nothing


getFlightsR :: Handler RepHtml
getFlightsR = do
  flights <- runDB $ selectList [] []
  (newFlightWidget, enctype) <- generateFormPost flightForm
  defaultLayout $(whamletFile "templates/flights/index.hamlet")


postFlightsR :: Handler RepHtml
postFlightsR = do
  ((result, widget), enctype) <- runFormPost flightForm
  case result of
    FormSuccess flight -> do
                 runDB $ insert flight
                 redirect FlightsR
    _ -> redirect FlightsR


getFlightR :: FlightId -> Handler RepHtml
getFlightR id = do
  flight <- runDB $ get404 id
  defaultLayout $(whamletFile "templates/flights/show.hamlet")


main :: IO ()
main = do
  config <- loadConfig "config/config.yml"
  withSqlitePool (sqlitePath config) (sqliteConnectionPoolCount config) $ \pool -> do
         runSqlPool (runMigration migrateAll) pool
         warpDebug 3000 $ Volare pool
