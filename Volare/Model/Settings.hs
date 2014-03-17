module Volare.Model.Settings (settings) where

import Control.Applicative ((<$>))
import Database.Persist.Class (entityIdFromJSON,
                               entityIdToJSON)
import Database.Persist.TH (EntityJSON,
                            MkPersistSettings,
                            entityFromJSON,
                            entityToJSON,
                            mpsEntityJSON,
                            sqlSettings)

settings :: MkPersistSettings
settings = sqlSettings {
    -- TODO
    -- Use EntityJSON data constructor once it's exported
    mpsEntityJSON = replaceEntityJSON <$> mpsEntityJSON sqlSettings
}


replaceEntityJSON :: EntityJSON ->
                     EntityJSON
replaceEntityJSON entityJSON = entityJSON {
    entityToJSON = 'entityIdToJSON,
    entityFromJSON = 'entityIdFromJSON
}
