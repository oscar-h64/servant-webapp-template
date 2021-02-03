--------------------------------------------------------------------------------
-- Servant Webapp Template                                                    --
--------------------------------------------------------------------------------
-- This source code is licensed under the BSD3 licence found in the LICENSE   --
-- file in the root directory of this source tree.                            --
--                                                                            --
-- Copyright 2020 Oscar Harris (oscar@oscar-h.com)                            --
--------------------------------------------------------------------------------

{-# LANGUAGE QuasiQuotes #-}

module App.UI.Form (
    FormElement(..),
    FormElementType(..),
    Form,
    renderForm
) where

--------------------------------------------------------------------------------

import Data.Text   ( Text )

import Text.Hamlet

--------------------------------------------------------------------------------

data FormElement = MkFormElement Text FormElementType

-- TODO: More form elements
data FormElementType
    = TextInput Text (Maybe Text) -- Placeholder, value
    | PasswordInput Text (Maybe Text) -- Placeholder, value

type Form = [FormElement]

--------------------------------------------------------------------------------

maybeToAttrib :: Text -> Maybe Text -> [(Text, Text)]
maybeToAttrib attrib (Just v) = [("value", v)]
maybeToAttrib _      Nothing  = []

-- TODO: Boostrap this
renderForm :: Text -> Form -> Html
renderForm name items = [shamlet|
<form name="#{name}" method=post>
    $forall MkFormElement itemName item <- items
        $case item
            $of TextInput placeholder mValue
                <input type=text placeholder="#{placeholder}" *{maybeToAttrib "value" mValue} />
            $of PasswordInput placeholder mValue
                <input type=password placeholder="#{placeholder}" *{maybeToAttrib "value" mValue} />
    <p> DO CSRF
    <p> DO SUBMIT
|]

--------------------------------------------------------------------------------
