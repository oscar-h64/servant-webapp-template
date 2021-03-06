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

import Servant

import App.Pages.Auth   ( AuthAPI, authHandlers )
import App.Pages.Home   ( HomeAPI, homeHandlers )
import App.Pages.Page1  ( Page1API, page1Handlers )
import App.Types.Common ( AppServer )

--------------------------------------------------------------------------------

type AppAPI =
      HomeAPI
 :<|> "page1" :> Page1API
 :<|> "auth" :> AuthAPI
 :<|> "static" :> Raw

appHandlers :: AppServer AppAPI
appHandlers = homeHandlers
         :<|> page1Handlers
         :<|> authHandlers
         :<|> serveDirectoryWebApp "static/"

--------------------------------------------------------------------------------
