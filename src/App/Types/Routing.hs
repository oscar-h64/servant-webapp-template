--------------------------------------------------------------------------------
-- Servant Webapp Template                                                    --
--------------------------------------------------------------------------------
-- This source code is licensed under the BSD3 licence found in the LICENSE   --
-- file in the root directory of this source tree.                            --
--                                                                            --
-- Copyright 2020 Oscar Harris (oscar@oscar-h.com)                            --
--------------------------------------------------------------------------------

module App.Types.Routing (
    Page(..)
) where

--------------------------------------------------------------------------------

data ShowInNav = Always | OnlyWhenAuthed | Never

data Page showInNav name path requiresAuth where
    Home  :: Page Always "Home" "/" False
    Login :: Page Always "Login" "/login" False

--------------------------------------------------------------------------------
