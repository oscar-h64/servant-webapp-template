--------------------------------------------------------------------------------
-- Servant Webapp Template                                                    --
--------------------------------------------------------------------------------
-- This source code is licensed under the BSD3 licence found in the LICENSE   --
-- file in the root directory of this source tree.                            --
--                                                                            --
-- Copyright 2020 Oscar Harris (oscar@oscar-h.com)                            --
--------------------------------------------------------------------------------

module Main where

--------------------------------------------------------------------------------

import Control.Concurrent                  ( forkIO )
import Control.Monad                       ( forM_, void )
import Control.Monad.Logger                ( runStderrLoggingT )
import Control.Monad.Trans.Reader          ( runReaderT )

import Data.Maybe                          ( fromJust, fromMaybe, listToMaybe )
import Data.Proxy                          ( Proxy (..) )
import Data.Text                           ( pack )
import Data.Text.Encoding                  ( encodeUtf8 )
import Data.Yaml                           ( decodeFileThrow )

import Database.Persist.Postgresql         ( createPostgresqlPool, runMigration,
                                             runSqlPool )

import Network.Wai.Handler.Warp            ( defaultSettings, run, setPort )
import Network.Wai.Handler.WarpTLS         ( runTLS, tlsSettings )
import Network.Wai.Middleware.EnforceHTTPS

import Servant                             ( Context ((:.), EmptyContext),
                                             Server, hoistServerWithContext,
                                             serveWithContext )
import Servant.Auth.Server                 ( CookieSettings, JWTSettings,
                                             SameSite (SameSiteStrict),
                                             cookieSameSite, cookieXsrfSetting,
                                             defaultCookieSettings,
                                             defaultJWTSettings,
                                             defaultXsrfCookieSettings, readKey,
                                             xsrfExcludeGet )

import System.Environment                  ( getArgs )

import App
import App.Types.Config
import App.Types.Database    ( migrateAll )
import App.Types.Environment

--------------------------------------------------------------------------------

-- | This function defines how to run the AppServer applications
appToServer :: Environment -> Server AppAPI
appToServer cfg = hoistServerWithContext
    (Proxy @AppAPI)
    (Proxy @'[CookieSettings, JWTSettings])
    (`runReaderT` cfg)
    appHandlers

-- | Entrypoint for server
main :: IO ()
main = do
    -- read config from the file stated in the first argument, or
    -- config/config.yaml if none given
    configFile <- fromMaybe "config/config.yaml" . listToMaybe <$> getArgs
    MkConfig MkServerConfig{..} MkDBConfig{..} <- decodeFileThrow configFile

    -- components to join to form DB connection string
    let dbStr = [ "host=", dbHost
                , " port=", pack $ show dbPort
                , " user=", dbUser
                , " password=", dbPassword
                , " dbname=", dbDb
                ]

    -- open database connection
    sqlPool <- runStderrLoggingT
             $ createPostgresqlPool (encodeUtf8 $ mconcat dbStr) dbPools

    -- run automatic migrations
    runSqlPool (runMigration migrateAll) sqlPool

    -- generate config for JWT/cookie
    jwtKey <- readKey serverJwtKey
    let jwtCfg = defaultJWTSettings jwtKey
    let cookieCfg = defaultCookieSettings{
            cookieSameSite = SameSiteStrict,
            cookieXsrfSetting = Nothing
        }
    let cfg = cookieCfg :. jwtCfg :. EmptyContext

    -- Create basic app
    let app = serveWithContext (Proxy @AppAPI) cfg
            $ appToServer
            $ MkEnvironment sqlPool jwtCfg cookieCfg

    -- Run server, either using a resolver or running an HTTPS server
    -- depending on the config
    case serverHttpsConf of
        Left MkHTTPSResolverConf{..} -> do
            -- Enforce HTTPS via given resolver
            let app' = case serverResolver of
                    "forwarded" -> withResolver forwarded app
                    "xForwardedProto" -> withResolver xForwardedProto app
                    _ -> error "Invalid resolver"
            
            run serverPort app'

        Right MkHTTPSActualConf{..} -> do
            let app' = withConfig defaultConfig{httpsPort = serverPort} app

            -- run HTTP redirect server if serverHTTPPort is `Just`
            forM_ serverHttpPort $ \p -> do
                putStrLn $ "Started HTTP server on " ++ show p
                forkIO $ run p app'

            -- run HTTPS server with port, certificate and key listed in config
            -- file
            putStrLn $ "Started HTTPS server on " ++ show serverPort
            let tlsCfg = tlsSettings serverCert serverKey
            runTLS tlsCfg (setPort serverPort defaultSettings) app

--------------------------------------------------------------------------------
