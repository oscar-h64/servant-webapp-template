--------------------------------------------------------------------------------
-- Servant Webapp Template                                                    --
--------------------------------------------------------------------------------
-- This source code is licensed under the BSD3 licence found in the LICENSE   --
-- file in the root directory of this source tree.                            --
--                                                                            --
-- Copyright 2020 Oscar Harris (oscar@oscar-h.com)                            --
--------------------------------------------------------------------------------

module App.Util.Auth (
    requireLoggedIn
) where

--------------------------------------------------------------------------------

import Servant.Auth.Server ( AuthResult (..) )

import App.Types.Common    ( AppHandler, AppServer )
import App.Util.Error      ( error401 )

--------------------------------------------------------------------------------

requireLoggedIn :: (a -> AppHandler b) -> AuthResult a -> AppHandler b
requireLoggedIn f (Authenticated a) = f a
requireLoggedIn _ _                 = error401

--------------------------------------------------------------------------------
