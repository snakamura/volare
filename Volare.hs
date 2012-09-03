{-# LANGUAGE FlexibleContexts,
             GADTs,
             MultiParamTypeClasses,
             OverloadedStrings,
             QuasiQuotes,
             TemplateHaskell,
             TypeFamilies #-}

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
import Yesod (FormMessage,
              FormResult(FormSuccess),
              Html,
              MForm,
              RenderMessage,
              RepHtml,
              Yesod,
              YesodPersist(..),
              areq,
              defaultFormMessage,
              defaultLayout,
              generateFormPost,
              getYesod,
              mkYesod,
              parseRoutes,
              redirect,
              renderDivs,
              renderMessage,
              renderRoute,
              runFormPost,
              textField,
              warpDebug,
              whamletFile,
              yesodDispatch)


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persist|
Flight
  name T.Text
  deriving Show
|]


data Volare = Volare ConnectionPool


mkYesod "Volare" [parseRoutes|
/ RootR GET
/flights FlightsR GET POST
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


main :: IO ()
main = withSqlitePool "volare.sqlite" 10 $ \pool -> do
         runSqlPool (runMigration migrateAll) pool
         warpDebug 3000 $ Volare pool
