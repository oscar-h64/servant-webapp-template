--------------------------------------------------------------------------------
-- Servant Webapp Template                                                    --
--------------------------------------------------------------------------------
-- This source code is licensed under the BSD3 licence found in the LICENSE   --
-- file in the root directory of this source tree.                            --
--                                                                            --
-- Copyright 2020 Oscar Harris (oscar@oscar-h.com)                            --
--------------------------------------------------------------------------------

module App.Types.Common (
    JSONStripPrefix,

    AppHandler,
    AppServer,

    RequireAuth,
    AppHandlerAuth,

    Webpage,
    Webpage',
    Redirect,
    Redirect',

    EmailChannel,

    Environment(..)
) where

--------------------------------------------------------------------------------

import Control.Concurrent          ( Chan )
import Control.Monad.Trans.Reader  ( ReaderT )

import Database.Persist.Postgresql ( ConnectionPool )

import Deriving.Aeson              ( CamelToKebab, CustomJSON,
                                     FieldLabelModifier, RejectUnknownFields,
                                     StripPrefix )

import GHC.TypeLits                ( Symbol )

import Network.Mail.Mime           ( Address, Mail )

import Servant
import Servant.Auth                ( Auth, Cookie )
import Servant.Auth.Server         ( AuthResult, CookieSettings, FromJWT,
                                     JWTSettings, ToJWT )
import Servant.HTML.Blaze          ( HTML )
import Servant.Server              ( Handler, HasServer (ServerT) )

import Text.Hamlet                 ( Html )

--------------------------------------------------------------------------------

-- JSON:

type JSONStripPrefix (str :: Symbol) =
    CustomJSON '[ FieldLabelModifier (StripPrefix str, CamelToKebab)
                -- , RejectUnknownFields -- TODO: Why does this fail
                ]

--------------------------------------------------------------------------------

-- Monads:

type AppHandler = ReaderT Environment Handler

type AppServer api = ServerT api AppHandler

--------------------------------------------------------------------------------

-- Auth:

type RequireAuth = Auth '[Cookie] Int

-- The RequireAuth combinator should go last in order to use AppHandlerAuth
type AppHandlerAuth a = AuthResult Int -> AppHandler a

instance FromJWT Int
instance ToJWT Int

--------------------------------------------------------------------------------

-- Routing: These are quick aliases for use in API types. The ' versions allow
-- specifying a custom return type, which is useful for adding headers etc.

type Webpage = Get '[HTML] Html
type Webpage' a = Get '[HTML] a

type Redirect = Verb POST 303 '[HTML] NoContent
type Redirect' a = Verb POST 303 '[HTML] a

--------------------------------------------------------------------------------

-- Email:

type EmailChannel = Chan Mail

--------------------------------------------------------------------------------

-- Environment:

data Environment = MkEnvironment {
    envConnectionPool   :: ConnectionPool,
    envJWTConfig        :: JWTSettings,
    envCookieConfig     :: CookieSettings,
    envEmailChannel     :: EmailChannel,
    envEmailDefaultFrom :: Address
}

--------------------------------------------------------------------------------
