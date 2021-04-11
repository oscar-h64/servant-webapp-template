--------------------------------------------------------------------------------
-- Servant Webapp Template                                                    --
--------------------------------------------------------------------------------
-- This source code is licensed under the BSD3 licence found in the LICENSE   --
-- file in the root directory of this source tree.                            --
--                                                                            --
-- Copyright 2020 Oscar Harris (oscar@oscar-h.com)                            --
--------------------------------------------------------------------------------

{-# LANGUAGE TemplateHaskell #-}

module App.Util.Error (
    error401,
    error403,
    notFoundFormatter
) where

--------------------------------------------------------------------------------

import Servant

import Text.Blaze.Renderer.Utf8 ( renderMarkup )

import App.Types.Common         ( AppHandler, AppServer )
import App.Types.Routing        ( Page (..) )
import App.UI                   ( hamletFile, makePage, makePage', redirect )

--------------------------------------------------------------------------------

error401 :: AppHandler a
error401 = redirect Login

error403 :: Maybe Page -> AppHandler a
error403 mPage = do
    page <- makePage "Permission Denied" mPage $(hamletFile "errors/403")
    throwError $ err403 { errBody = renderMarkup page }

notFoundFormatter :: NotFoundErrorFormatter
notFoundFormatter _ =
    let page = makePage' "Not Found" Nothing $(hamletFile "errors/404") Nothing
    in err404 { errBody = renderMarkup page }

--------------------------------------------------------------------------------
