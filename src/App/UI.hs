--------------------------------------------------------------------------------
-- Servant Webapp Template                                                    --
--------------------------------------------------------------------------------
-- This source code is licensed under the BSD3 licence found in the LICENSE   --
-- file in the root directory of this source tree.                            --
--                                                                            --
-- Copyright 2020 Oscar Harris (oscar@oscar-h.com)                            --
--------------------------------------------------------------------------------

{-# LANGUAGE TemplateHaskell #-}

module App.UI (
    hamletFile,
    makePage,
    makePage',
    redirect
) where

--------------------------------------------------------------------------------

import           Data.Maybe         ( fromMaybe, isJust )
import           Data.Text          ( Text )
import           Data.Text.Encoding ( encodeUtf8 )

import           Servant            ( ServerError (errHeaders), err303,
                                      throwError )

import           Text.Hamlet        ( Html, HtmlUrl )
import qualified Text.Hamlet        as H ( hamletFile )

import           App.Types.Common
import           App.Types.Routing

--------------------------------------------------------------------------------

hamletFile p = H.hamletFile $ "templates/" <> p <> ".hamlet"

makePage :: Text -> Maybe Page -> HtmlUrl Page -> AppHandler Html
makePage title mPage pageContent = makePage' title mPage pageContent <$> getUser

makePage' :: Text -> Maybe Page -> HtmlUrl Page -> Maybe AuthedUser -> Html
makePage' title mPage pageContent mUser =
    let renderFunc page _ = getUrl page
        showNavItem y = pdShowInNav y == Always
                     || (isJust mUser && pdShowInNav y == OnlyWhenAuthed)
        navItems = [(x,y) | x <- [minBound..maxBound]
                          , let y = pageData x
                          , showNavItem y
                   ]
    in $(H.hamletFile "templates/base/layout.hamlet") renderFunc

--------------------------------------------------------------------------------

-- | `redirect` @url@ short circuits the AppHandler monad, throwing an HTTP303
-- response which redirects to the page @url@
redirect :: HasUrl u => u -> AppHandler a
redirect url = throwError
              $ err303 { errHeaders = [( "Location"
                                       , encodeUtf8 $ getUrl url
                                       )] }

--------------------------------------------------------------------------------
