--------------------------------------------------------------------------------
-- Servant Webapp Template                                                    --
--------------------------------------------------------------------------------
-- This source code is licensed under the BSD3 licence found in the LICENSE   --
-- file in the root directory of this source tree.                            --
--                                                                            --
-- Copyright 2020 Oscar Harris (oscar@oscar-h.com)                            --
--------------------------------------------------------------------------------

{-# LANGUAGE TemplateHaskell #-}

module App.Pages.Page1 (
    Page1API,
    page1Handlers
) where

--------------------------------------------------------------------------------

import Servant           ( (:>) )

import Text.Hamlet       ( Html )

import App.Types.Common
import App.Types.Routing
import App.UI
import App.Util.Auth
import App.Util.Error

--------------------------------------------------------------------------------

type Page1API = Webpage

page1 :: EndpointHandler Html
page1 = requireLoggedIn $ \_ -> do
    makePage "Page 1" (pure Page1) $(hamletFile "page1")

page1Handlers :: AppServer Page1API
page1Handlers = page1

--------------------------------------------------------------------------------
