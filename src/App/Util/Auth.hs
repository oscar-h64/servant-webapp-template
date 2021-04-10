--------------------------------------------------------------------------------
-- Servant Webapp Template                                                    --
--------------------------------------------------------------------------------
-- This source code is licensed under the BSD3 licence found in the LICENSE   --
-- file in the root directory of this source tree.                            --
--                                                                            --
-- Copyright 2020 Oscar Harris (oscar@oscar-h.com)                            --
--------------------------------------------------------------------------------

module App.Util.Auth (
    authNotRequired,
    requireLoggedIn
) where

--------------------------------------------------------------------------------

import Control.Monad.Reader

import Servant.Auth.Server  ( AuthResult (..) )

import App.Types.Common     ( AppHandler, AuthedUser, EndpointHandler, getEnv )
import App.Util.Error       ( error401 )

--------------------------------------------------------------------------------

authResultToMaybe :: AuthResult a -> Maybe a
authResultToMaybe (Authenticated a) = Just a
authResultToMaybe _                 = Nothing

authNotRequired :: AppHandler b -> EndpointHandler b
authNotRequired f authRes = do
    env <- getEnv
    lift $ runReaderT f (authResultToMaybe authRes, env)

requireLoggedIn :: (AuthedUser -> AppHandler b) -> EndpointHandler b
requireLoggedIn f (Authenticated a) = do
    env <- getEnv
    lift $ runReaderT (f a) (Just a, env)
requireLoggedIn _ _                 = error401

--------------------------------------------------------------------------------
