--------------------------------------------------------------------------------
-- Servant Webapp Template                                                    --
--------------------------------------------------------------------------------
-- This source code is licensed under the BSD3 licence found in the LICENSE   --
-- file in the root directory of this source tree.                            --
--                                                                            --
-- Copyright 2020 Oscar Harris (oscar@oscar-h.com)                            --
--------------------------------------------------------------------------------

module App.UI.Table (
    module Colonnade,
    module Text.Blaze.Colonnade,

    BootstrapColonnade,
    bootstrapTable
) where

--------------------------------------------------------------------------------

import Prelude                     hiding ( div )

import Colonnade

import Text.Blaze                  ( Attribute, customAttribute, dataAttribute )
import Text.Blaze.Colonnade
import Text.Blaze.Html5            ( Html, div, (!) )
import Text.Blaze.Html5.Attributes ( class_ )

--------------------------------------------------------------------------------

type BootstrapColonnade a = Colonnade Headed a Cell

bootstrapTable :: Foldable f => Colonnade Headed a Cell -> f a -> Html
bootstrapTable col xs = div ! class_ "table-responsive-sm"
                      $ encodeCellTable (class_ "table table-hover") col xs

--------------------------------------------------------------------------------
