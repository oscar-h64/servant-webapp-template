--------------------------------------------------------------------------------
-- Servant Webapp Template                                                    --
--------------------------------------------------------------------------------
-- This source code is licensed under the BSD3 licence found in the LICENSE   --
-- file in the root directory of this source tree.                            --
--                                                                            --
-- Copyright 2020 Oscar Harris (oscar@oscar-h.com)                            --
--------------------------------------------------------------------------------

{-# LANGUAGE TemplateHaskell #-}

module App.UI.Alert (
    AlertType(..),
    Alert(..),

    renderAlert
) where

--------------------------------------------------------------------------------

import Data.Text         ( Text )

import Text.Hamlet       ( Html, HtmlUrl )

import App.Types.Routing
import App.UI

--------------------------------------------------------------------------------

-- These match the bootstrap types
data AlertType = Primary
               | Secondary
               | Success
               | Danger
               | Warning
               | Info
               | Light
               | Dark

alertTypeToText :: AlertType -> Text
alertTypeToText Primary   = "primary"
alertTypeToText Secondary = "secondary"
alertTypeToText Success   = "success"
alertTypeToText Danger    = "danger"
alertTypeToText Warning   = "warning"
alertTypeToText Info      = "info"
alertTypeToText Light     = "light"
alertTypeToText Dark      = "dark"

data Alert = MkAlert {
    alertType        :: AlertType,
    alertContent     :: HtmlUrl Page,
    alertDismissable :: Bool
}

--------------------------------------------------------------------------------

renderAlert :: Alert -> HtmlUrl Page
renderAlert MkAlert{..} = $(hamletFile "components/alert")

--------------------------------------------------------------------------------
