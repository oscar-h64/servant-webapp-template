--------------------------------------------------------------------------------
-- Servant Webapp Template                                                    --
--------------------------------------------------------------------------------
-- This source code is licensed under the BSD3 licence found in the LICENSE   --
-- file in the root directory of this source tree.                            --
--                                                                            --
-- Copyright 2020 Oscar Harris (oscar@oscar-h.com)                            --
--------------------------------------------------------------------------------

module App.Types.Config (
    Config(..),
    ServerConfig(..),
    HTTPSResolverConf(..),
    HTTPSActualConf(..),
    DBConfig(..)
) where

--------------------------------------------------------------------------------

import Data.Text ( Text )

import Deriving.Aeson ( CustomJSON(..), FromJSON, Generic )

import App.Types.Common ( JSONStripPrefix )

--------------------------------------------------------------------------------

-- | The settings for the server, made up of the port to run on and the database
-- configuration
data Config = MkConfig {
    cfgServer :: ServerConfig,
    cfgDb     :: DBConfig
} deriving Generic
  deriving FromJSON via JSONStripPrefix "cfg" Config

-- | The settings for the webserver
data ServerConfig = MkServerConfig {
    serverJwtKey    :: FilePath,
    serverPort      :: Int,
    serverHttpsConf :: Either HTTPSResolverConf HTTPSActualConf
} deriving Generic
  deriving FromJSON via JSONStripPrefix "server" ServerConfig

newtype HTTPSResolverConf = MkHTTPSResolverConf {
    serverResolver :: Text
} deriving Generic
  deriving FromJSON via JSONStripPrefix "server" HTTPSResolverConf

data HTTPSActualConf = MkHTTPSActualConf {
    serverHttpPort :: Maybe Int,
    serverCert     :: FilePath,
    serverKey      :: FilePath
} deriving Generic
  deriving FromJSON via JSONStripPrefix "server" HTTPSActualConf

-- | The settings for the database connections
data DBConfig = MkDBConfig {
    dbHost     :: Text,
    dbPort     :: Int,
    dbDb       :: Text,
    dbUser     :: Text,
    dbPassword :: Text,
    dbPools    :: Int
} deriving Generic
  deriving FromJSON via JSONStripPrefix "db" DBConfig

--------------------------------------------------------------------------------
