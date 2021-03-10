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

import Control.Monad.IO.Class ( liftIO )
import Control.Monad.Reader   ( ask )

import Servant
import Servant.Auth.Server    ( SetCookie, acceptLogin )
import Servant.HTML.Blaze     ( HTML )

import Text.Hamlet            ( Html )

import App.Types.Common
import App.Types.Routing
import App.UI
import App.UI.Form
import App.Util.Error

--------------------------------------------------------------------------------

type AuthAPI = "login" :> ( Webpage
                       :<|> Post '[HTML] (AuthCookies Html)
                          )

type AuthCookies a = Headers '[ Header "Set-Cookie" SetCookie
                              , Header "Set-Cookie" SetCookie
                              ]
                             a

loginPage :: AppHandler Html
loginPage = makePage "Login" (pure Login) $(hamletFile "login")
    where loginForm = renderForm $ MkForm "loginForm" (Just "Login")
              [ MkFormElement "username" $ TextInput Plain    "Username" Nothing
              , MkFormElement "password" $ TextInput Password "Password" Nothing
              ]

loginPost :: AppHandler (AuthCookies Html)
loginPost = do
    env <- ask
    mApplyCookies <- liftIO $ acceptLogin (envCookieConfig env)
                                          (envJWTConfig env)
                                          (0 :: Int)

    maybe error401 (<$> loginPage) mApplyCookies

authHandlers :: AppServer AuthAPI
authHandlers = loginPage :<|> loginPost

--------------------------------------------------------------------------------
