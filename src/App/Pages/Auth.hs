--------------------------------------------------------------------------------
-- Servant Webapp Template                                                    --
--------------------------------------------------------------------------------
-- This source code is licensed under the BSD3 licence found in the LICENSE   --
-- file in the root directory of this source tree.                            --
--                                                                            --
-- Copyright 2020 Oscar Harris (oscar@oscar-h.com)                            --
--------------------------------------------------------------------------------

{-# LANGUAGE TemplateHaskell #-}

module App.Pages.Auth (
    AuthAPI,
    authHandlers
) where

--------------------------------------------------------------------------------

import Text.Hamlet       ( Html, hamletFile )

import Servant           ( (:>) )

import App.Types.Common
import App.Types.Monad
import App.Types.Routing
import App.UI
import App.UI.Form

--------------------------------------------------------------------------------

type AuthAPI = "login" :> Webpage

loginPage :: AppHandler Html
loginPage = makePage "Login" (pure Login) $(hamletFile "templates/login.hamlet")
    where loginForm = renderForm $ MkForm "loginForm" (Just "Login")
              [ MkFormElement "username" $ TextInput Plain    "Username" Nothing
              , MkFormElement "password" $ TextInput Password "Password" Nothing
              ]

authHandlers :: AppServer AuthAPI
authHandlers = loginPage

--------------------------------------------------------------------------------
