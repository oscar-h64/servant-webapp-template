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
    pageData,
    getPagePath
) where

--------------------------------------------------------------------------------

import Data.Text ( Text )

--------------------------------------------------------------------------------

data ShowInNav = Always | OnlyWhenAuthed | Never
    deriving Eq

data Page = Home | Page1 | Login
    deriving (Eq, Enum, Bounded)

data PageData = MkPageData {
    pdShowInNav :: ShowInNav,
    pdNavName   :: Text,
    pdPath      :: Text,
    pdSubpages  :: [Page]
}

pageData :: Page -> PageData
pageData Home  = MkPageData Always "Home" "/" []
pageData Page1 = MkPageData Always "Page 1" "/page1" []
pageData Login = MkPageData Never "Login" "/auth/login" []

getPagePath :: Page -> Text
getPagePath = pdPath . pageData

--------------------------------------------------------------------------------
