--------------------------------------------------------------------------------
-- Servant Webapp Template                                                    --
--------------------------------------------------------------------------------
-- This source code is licensed under the BSD3 licence found in the LICENSE   --
-- file in the root directory of this source tree.                            --
--                                                                            --
-- Copyright 2020 Oscar Harris (oscar@oscar-h.com)                            --
--------------------------------------------------------------------------------

module App.Pages.Home (
    HomeAPI,
    homeHandlers
) where

--------------------------------------------------------------------------------

import Servant            ( Get )
import Servant.HTML.Blaze ( HTML )

import Text.Hamlet        ( Html )

import App.Types.Monad

--------------------------------------------------------------------------------

-- TODO: Move this
type Webpage = Get '[HTML] Html

type HomeAPI = Webpage

homeHandlers :: AppServer HomeAPI
homeHandlers = undefined

--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
