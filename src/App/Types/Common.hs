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
    Webpage,
    EmailChannel
) where

--------------------------------------------------------------------------------

import Control.Concurrent ( Chan )

import Deriving.Aeson     ( CamelToKebab, CustomJSON, FieldLabelModifier,
                            RejectUnknownFields, StripPrefix )

import GHC.TypeLits       ( Symbol )

import Network.Mail.Mime  ( Mail )

import Servant            ( Get )
import Servant.HTML.Blaze ( HTML )

import Text.Hamlet        ( Html )

--------------------------------------------------------------------------------

type JSONStripPrefix (str :: Symbol) =
    CustomJSON '[ FieldLabelModifier (StripPrefix str, CamelToKebab)
                -- , RejectUnknownFields -- TODO: Why does this fail
                ]

--------------------------------------------------------------------------------

type Webpage = Get '[HTML] Html

--------------------------------------------------------------------------------

type EmailChannel = Chan Mail

--------------------------------------------------------------------------------
