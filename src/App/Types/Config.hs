--------------------------------------------------------------------------------
-- Servant Webapp Template                                                    --
--------------------------------------------------------------------------------
-- This source code is licensed under the BSD3 licence found in the LICENSE   --
-- file in the root directory of this source tree.                            --
--                                                                            --
-- Copyright 2020 Oscar Harris (oscar@oscar-h.com)                            --
--------------------------------------------------------------------------------

module App.Types.Config (
    CmdOpts(..),
    cmdOpts,
    Config(..),
    ServerConfig(..),
    HTTPSConfig(..),
    DBConfig(..),
    SMTPConfig(..)
) where

--------------------------------------------------------------------------------

import Data.Text           ( Text )

import Data.Aeson          ( FromJSON (..), withObject, (.:), (.:?) )
import Deriving.Aeson      ( CustomJSON (..), FromJSON, Generic )

import Network.Mail.SMTP   ( Address (Address) )

import Options.Applicative

import App.Types.Common    ( JSONStripPrefix )

--------------------------------------------------------------------------------

data CmdOpts = MkCmdOpts {
    optConfigFile :: FilePath,
    optMigrate    :: Bool
}

cmdOpts :: ParserInfo CmdOpts
cmdOpts = info (opts <**> helper) fullDesc
  where opts = MkCmdOpts <$> strOption (short 'c'
                                         <> long "config"
                                         <> help "Location of the config file"
                                         <> value "config/config.yaml"
                                         <> showDefault)
                         <*> switch (long "migrate"
                                      <> help "Whether to migrate the DB")

--------------------------------------------------------------------------------

-- | The settings for the server, made up of the port to run on and the database
-- configuration
data Config = MkConfig {
    cfgServer :: ServerConfig,
    cfgDb     :: DBConfig,
    cfgSmtp   :: Maybe SMTPConfig
} deriving Generic
  deriving FromJSON via JSONStripPrefix "cfg" Config

-- | The settings for the webserver
data ServerConfig = MkServerConfig {
    serverJwtKey      :: FilePath,
    serverPort        :: Int,
    serverResolver    :: Maybe Text,
    serverHttpsConfig :: Maybe HTTPSConfig
} deriving Generic
  deriving FromJSON via JSONStripPrefix "server" ServerConfig

data HTTPSConfig = MkHTTPSConfig {
    serverHttpPort :: Maybe Int,
    serverCert     :: FilePath,
    serverKey      :: FilePath
} deriving Generic
  deriving FromJSON via JSONStripPrefix "server" HTTPSConfig

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

-- | The settings for the SMTP server
data SMTPConfig = MkSMTPConfig {
    smtpHostname    :: String,
    smtpUsername    :: String,
    smtpPassword    :: String,
    smtpDefaultFrom :: Maybe Address
} deriving Generic
  deriving FromJSON via JSONStripPrefix "smtp" SMTPConfig

instance FromJSON Address where
    parseJSON = withObject "Address" $ \v ->
        Address <$> v .:? "name"
                <*> v .:  "address"

--------------------------------------------------------------------------------
