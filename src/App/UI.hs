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
    redirect,
    redirect'
) where

--------------------------------------------------------------------------------

import           Data.Text          ( Text )
import           Data.Text.Encoding ( encodeUtf8 )

import           Servant            ( ServerError (errHeaders), err303,
                                      throwError )

import           Text.Hamlet        ( Html, HtmlUrl )
import qualified Text.Hamlet        as H ( hamletFile )

import           App.Types.Common   ( AppHandler )
import           App.Types.Routing  ( Page (..), PageData (..), ShowInNav (..),
                                      getPagePath, pageData )

--------------------------------------------------------------------------------

hamletFile p = H.hamletFile $ "templates/" <> p <> ".hamlet"

makePage :: Text -> Maybe Page -> HtmlUrl Page -> AppHandler Html
makePage title mPage pageContent =
    let renderFunc page _ = getPagePath page
        navItems = [(x,y) | x <- [minBound..maxBound]
                          , let y = pageData x
                          , pdShowInNav y == Always
                   ]
    in pure $ $(H.hamletFile "templates/base/layout.hamlet") renderFunc

-- | `redirect` @page@ short circuits the AppHandler monad, throwing an HTTP303
-- response which redirects to the page @page@
redirect :: Page -> AppHandler a
redirect page = throwError
              $ err303 { errHeaders = [( "Location"
                                       , encodeUtf8 $ getPagePath page
                                       )] }

-- | `redirect'` @url@ short circuits the AppHandler monad, throwing an HTTP303
-- response which redirects to the URL @url@
redirect' :: Text -> AppHandler a
redirect' url = throwError
              $ err303 { errHeaders = [("Location", encodeUtf8 url)] }

--------------------------------------------------------------------------------
