--------------------------------------------------------------------------------
-- Servant Webapp Template                                                    --
--------------------------------------------------------------------------------
-- This source code is licensed under the BSD3 licence found in the LICENSE   --
-- file in the root directory of this source tree.                            --
--                                                                            --
-- Copyright 2020 Oscar Harris (oscar@oscar-h.com)                            --
--------------------------------------------------------------------------------

{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module App.Types.Database (
    module ReExport,
    module App.Types.Database
) where

--------------------------------------------------------------------------------

import Data.ByteString.Char8       as BS ( pack, unpack )
import Data.UUID                   ( UUID, fromString, toString )

import Database.Persist
import Database.Persist            as ReExport ( Entity (..), Key (..) )
import Database.Persist.Postgresql
import Database.Persist.Sql
import Database.Persist.TH

--------------------------------------------------------------------------------

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|

|]

--------------------------------------------------------------------------------

instance PersistField UUID where
    toPersistValue = PersistDbSpecific . BS.pack . toString

    fromPersistValue (PersistDbSpecific bs) =
        case fromString $ BS.unpack bs of
            Just uuid -> Right uuid
            Nothing   -> Left "Invalid UUID"
    fromPersistValue _ = Left "Invalid UUID storage type"

instance PersistFieldSql UUID where
    sqlType _ = SqlOther "uuid"

--------------------------------------------------------------------------------
