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

import Data.Text              ( Text )

import Servant
import Servant.Auth.Server    ( AuthResult (..), SetCookie, acceptLogin,
                                clearSession )
import Servant.HTML.Blaze     ( HTML )

import Text.Hamlet            ( Html )

import Web.FormUrlEncoded

import App.Types.Common
import App.Types.Routing
import App.UI
import App.UI.Form
import App.Util.Auth
import App.Util.Error
import App.Util.Misc

--------------------------------------------------------------------------------

type AuthAPI =
      "login" :> LoginAPI
 :<|> "logout" :> LogoutAPI

type LoginAPI =
      Webpage
 :<|> Redirect' (AuthCookies '[Header "Location" Text] NoContent)

type LogoutAPI =
      ReqBody '[FormUrlEncoded] LogoutForm :>
      Redirect' (AuthCookies '[Header "Location" Text] NoContent)

type AuthCookies hs a = Headers (   Header "Set-Cookie" SetCookie
                                 ': Header "Set-Cookie" SetCookie
                                 ': hs
                                )
                                a

--------------------------------------------------------------------------------

handleLoginPage :: EndpointHandler Html
handleLoginPage (Authenticated _) = redirect Home
handleLoginPage _                 =
    let loginForm = renderForm $ MkForm "loginForm" (Just "Login")
            [ MkFormElement "username" $ TextInput Plain    "Username" Nothing
            , MkFormElement "password" $ TextInput Password "Password" Nothing
            ]
    in makePage "Login" (pure Login) $(hamletFile "login")

-- Temporary login endpoint - just logs in anyone that POSTs here
loginPost :: EndpointHandler (AuthCookies '[Header "Location" Text] NoContent)
loginPost = authNotRequired $ do
    env <- getEnv
    mApplyCookies <- liftIO $ acceptLogin (envCookieConfig env)
                                          (envJWTConfig env)
                                          (0 :: Int)

    applyAuthCookies <- maybe error401 pure mApplyCookies
    pure $ applyAuthCookies $ addHeader (getUrl Home) NoContent

--------------------------------------------------------------------------------

newtype LogoutForm = MkLogoutForm { logoutRedirectUrl :: Text }

instance FromForm LogoutForm where
    fromForm = fmap (MkLogoutForm . ensureRelativeUrl (getUrl Home))
             . parseUnique "redirectUrl"

logoutPost :: LogoutForm
           -> EndpointHandler (AuthCookies '[Header "Location" Text] NoContent)
logoutPost MkLogoutForm{..} = requireLoggedIn $ \_ -> do
    cookieConf <- askEnv envCookieConfig
    pure $ clearSession cookieConf $ addHeader logoutRedirectUrl NoContent

--------------------------------------------------------------------------------

authHandlers :: AppServer AuthAPI
authHandlers = (handleLoginPage :<|> loginPost) :<|> logoutPost

--------------------------------------------------------------------------------
