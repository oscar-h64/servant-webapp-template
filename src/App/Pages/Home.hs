--------------------------------------------------------------------------------
-- Servant Webapp Template                                                    --
--------------------------------------------------------------------------------
-- This source code is licensed under the BSD3 licence found in the LICENSE   --
-- file in the root directory of this source tree.                            --
--                                                                            --
-- Copyright 2020 Oscar Harris (oscar@oscar-h.com)                            --
--------------------------------------------------------------------------------

{-# LANGUAGE TemplateHaskell #-}

module App.Pages.Home (
    HomeAPI,
    homeHandlers
) where

--------------------------------------------------------------------------------

import App.Types.Common
import App.Types.Routing
import App.UI
import App.Util.Auth

--------------------------------------------------------------------------------

type HomeAPI = Webpage

homeHandlers :: AppServer HomeAPI
homeHandlers = authNotRequired
             $ makePage "Home" (pure Home) $(hamletFile "home")

--------------------------------------------------------------------------------
