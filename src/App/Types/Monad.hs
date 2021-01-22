--------------------------------------------------------------------------------
-- Servant Webapp Template                                                    --
--------------------------------------------------------------------------------
-- This source code is licensed under the BSD3 licence found in the LICENSE   --
-- file in the root directory of this source tree.                            --
--                                                                            --
-- Copyright 2020 Oscar Harris (oscar@oscar-h.com)                            --
--------------------------------------------------------------------------------

module App.Types.Monad (
    AppHandler,
    AppServer
) where

--------------------------------------------------------------------------------

import Control.Monad.Trans.Reader ( ReaderT )

import Servant.Server             ( Handler, HasServer (ServerT) )

import App.Types.Environment      ( Environment )

--------------------------------------------------------------------------------

type AppHandler = ReaderT Environment Handler

type AppServer api = ServerT api AppHandler

--------------------------------------------------------------------------------
