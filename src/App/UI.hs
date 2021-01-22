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
    makePage,
    redirect
) where

--------------------------------------------------------------------------------

import Data.ByteString               ( ByteString )

import Servant                       ( ServerError (errHeaders), err303,
                                       throwError )

import Text.Blaze.Html.Renderer.Utf8 ( renderHtml )
import Text.Hamlet                   ( shamletFile )

import App.Types.Monad               ( AppHandler )

--------------------------------------------------------------------------------

makePage pageContent = pure $(shamletFile "templates/base/layout.hamlet")

-- | `redirect` @url@ short circuits the AppHandler monad, throwing an HTTP303
-- response which redirects to @url@
redirect :: ByteString -> AppHandler a
redirect url = throwError $ err303 { errHeaders = [("Location", url)] }

--------------------------------------------------------------------------------
