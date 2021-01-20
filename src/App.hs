--------------------------------------------------------------------------------
-- Servant Webapp Template                                                    --
--------------------------------------------------------------------------------
-- This source code is licensed under the BSD3 licence found in the LICENSE   --
-- file in the root directory of this source tree.                            --
--                                                                            --
-- Copyright 2020 Oscar Harris (oscar@oscar-h.com)                            --
--------------------------------------------------------------------------------

module App (
    AppAPI,
    appHandlers
) where

--------------------------------------------------------------------------------

import Servant ( Raw, serveDirectoryWebApp )

import App.Types.Monad

--------------------------------------------------------------------------------

type AppAPI = Raw

appHandlers :: AppServer AppAPI
appHandlers = serveDirectoryWebApp "static/"

--------------------------------------------------------------------------------