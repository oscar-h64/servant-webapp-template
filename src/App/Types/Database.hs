--------------------------------------------------------------------------------
-- Servant Webapp Template                                                    --
--------------------------------------------------------------------------------
-- This source code is licensed under the BSD3 licence found in the LICENSE   --
-- file in the root directory of this source tree.                            --
--                                                                            --
-- Copyright 2020 Oscar Harris (oscar@oscar-h.com)                            --
--------------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE UndecidableInstances       #-}

module App.Types.Database (
    module ReExport,
    module App.Types.Database
) where

--------------------------------------------------------------------------------

import Control.Monad.IO.Class      ( MonadIO, liftIO )

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
    toPersistValue = PersistLiteralEscaped . BS.pack . toString

    fromPersistValue (PersistLiteralEscaped bs) =
        case fromString $ BS.unpack bs of
            Just uuid -> Right uuid
            Nothing   -> Left "Invalid UUID"
    fromPersistValue _ = Left "Invalid UUID storage type"

instance PersistFieldSql UUID where
    sqlType _ = SqlOther "uuid"

--------------------------------------------------------------------------------

class MonadIO m => HasDBConn m where
    getPool :: m ConnectionPool

runDB :: HasDBConn m => SqlPersistT IO a -> m a
runDB query = getPool >>= liftIO . runSqlPool query

--------------------------------------------------------------------------------
