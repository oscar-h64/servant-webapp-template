--------------------------------------------------------------------------------
-- Servant Webapp Template                                                    --
--------------------------------------------------------------------------------
-- This source code is licensed under the BSD3 licence found in the LICENSE   --
-- file in the root directory of this source tree.                            --
--                                                                            --
-- Copyright 2020 Oscar Harris (oscar@oscar-h.com)                            --
--------------------------------------------------------------------------------

module App.Types.Routing (
    ShowInNav(..),
    Page(..),
    PageData(..),
    pageData
) where

--------------------------------------------------------------------------------

import Data.Text ( Text )

--------------------------------------------------------------------------------

data ShowInNav = Always | OnlyWhenAuthed | Never
    deriving Eq

data Page = Home | Login
    deriving (Eq, Enum, Bounded)

data PageData = MkPageData {
    pdShowInNav    :: ShowInNav,
    pdNavName      :: Text,
    pdPath         :: Text,
    pdRequiresAuth :: Bool
}

pageData :: Page -> PageData
pageData Home  = MkPageData Always "Home" "/" False
pageData Login = MkPageData Never "Login" "/login" False

--------------------------------------------------------------------------------
