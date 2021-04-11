--------------------------------------------------------------------------------
-- Servant Webapp Template                                                    --
--------------------------------------------------------------------------------
-- This source code is licensed under the BSD3 licence found in the LICENSE   --
-- file in the root directory of this source tree.                            --
--                                                                            --
-- Copyright 2020 Oscar Harris (oscar@oscar-h.com)                            --
--------------------------------------------------------------------------------

module App.Util.Misc (
    ensureRelativeUrl
) where

--------------------------------------------------------------------------------

import Data.Text

import App.Types.Common ( AppHandler )

--------------------------------------------------------------------------------

ensureRelativeUrl :: Text -> Text -> Text
ensureRelativeUrl defaultUrl url =
    if "//" `isInfixOf` url
    then defaultUrl
    else url

--------------------------------------------------------------------------------
