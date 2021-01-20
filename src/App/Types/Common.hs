--------------------------------------------------------------------------------
-- Servant Webapp Template                                                    --
--------------------------------------------------------------------------------
-- This source code is licensed under the BSD3 licence found in the LICENSE   --
-- file in the root directory of this source tree.                            --
--                                                                            --
-- Copyright 2020 Oscar Harris (oscar@oscar-h.com)                            --
--------------------------------------------------------------------------------

module App.Types.Common (
    JSONStripPrefix
) where

--------------------------------------------------------------------------------

import Deriving.Aeson ( CustomJSON, FieldLabelModifier, StripPrefix )

import GHC.TypeLits   ( Symbol )

--------------------------------------------------------------------------------

type JSONStripPrefix (str :: Symbol) = CustomJSON '[FieldLabelModifier (StripPrefix str)]

--------------------------------------------------------------------------------
