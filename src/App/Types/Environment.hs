--------------------------------------------------------------------------------
-- Servant Webapp Template                                                    --
--------------------------------------------------------------------------------
-- This source code is licensed under the BSD3 licence found in the LICENSE   --
-- file in the root directory of this source tree.                            --
--                                                                            --
-- Copyright 2020 Oscar Harris (oscar@oscar-h.com)                            --
--------------------------------------------------------------------------------

module App.Types.Environment (
    Environment(..)
) where

--------------------------------------------------------------------------------

import Database.Persist.Postgresql ( ConnectionPool )

import Servant.Auth.Server         ( CookieSettings, JWTSettings )

--------------------------------------------------------------------------------

data Environment = MkEnvironment {
    envConnectionPool :: ConnectionPool,
    envJWTConfig      :: JWTSettings,
    envCookieConfig   :: CookieSettings
}

--------------------------------------------------------------------------------
