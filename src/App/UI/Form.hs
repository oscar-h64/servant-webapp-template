--------------------------------------------------------------------------------
-- Servant Webapp Template                                                    --
--------------------------------------------------------------------------------
-- This source code is licensed under the BSD3 licence found in the LICENSE   --
-- file in the root directory of this source tree.                            --
--                                                                            --
-- Copyright 2020 Oscar Harris (oscar@oscar-h.com)                            --
--------------------------------------------------------------------------------

{-# LANGUAGE TemplateHaskell #-}

module App.UI.Form (
    TextType(..),
    FormElement(..),
    FormElementType(..),
    Form(..),
    renderForm
) where

--------------------------------------------------------------------------------

import Data.Maybe        ( fromMaybe )
import Data.Text         ( Text )

import Text.Hamlet       ( HtmlUrl, hamletFile )

import App.Types.Routing

--------------------------------------------------------------------------------

data FormElement = MkFormElement Text FormElementType

data TextType = Plain | Email | Password

textTypeToInputType :: TextType -> Text
textTypeToInputType Plain    = "text"
textTypeToInputType Email    = "email"
textTypeToInputType Password = "password"

-- TODO: More form elements
data FormElementType = TextInput TextType Text (Maybe Text)
                     | HiddenInput Text

data Form = MkForm Text (Maybe Text) [FormElement] -- Name, Submit button text

--------------------------------------------------------------------------------

maybeToAttrib :: Text -> Maybe Text -> [(Text, Text)]
maybeToAttrib attrib (Just v) = [("value", v)]
maybeToAttrib _      Nothing  = []

renderForm :: Form -> HtmlUrl Page
renderForm (MkForm name mSubmitVal items) =
    $(hamletFile "templates/components/form.hamlet")

--------------------------------------------------------------------------------
