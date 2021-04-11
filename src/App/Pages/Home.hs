--------------------------------------------------------------------------------
-- Servant Webapp Template                                                    --
--------------------------------------------------------------------------------
-- This source code is licensed under the BSD3 licence found in the LICENSE   --
-- file in the root directory of this source tree.                            --
--                                                                            --
-- Copyright 2020 Oscar Harris (oscar@oscar-h.com)                            --
--------------------------------------------------------------------------------

{-# LANGUAGE TemplateHaskell #-}

module App.Pages.Home (
    HomeAPI,
    homeHandlers
) where

--------------------------------------------------------------------------------

import Data.Text

import Text.Hamlet       ( Html )

import App.Types.Common
import App.Types.Routing
import App.UI
import App.UI.Table
import App.Util.Auth

--------------------------------------------------------------------------------

data ExampleData = MkExampleData {
    edId      :: Int,
    edFname   :: Text,
    edSname   :: Text,
    edEnabled :: Bool
}

exampleData :: [ExampleData]
exampleData = [ MkExampleData 1 "John" "Smith" True
              , MkExampleData 3 "Elizabeth" "Jones" False
              ]

exampleColonnade :: BootstrapColonnade ExampleData
exampleColonnade = mconcat
    [ headed (textCell "ID") (stringCell . show . edId)
    , headed (textCell "Forename") (textCell . edFname)
    , headed (textCell "Surname") (textCell . edSname)
    , headed (textCell "Enabled") (stringCell . show . edEnabled)
    , headed (textCell "Dead Data 1") (const $ textCell "-")
    , headed (textCell "Dead Data 2") (const $ textCell "-")
    , headed (textCell "Dead Data 3") (const $ textCell "-")
    ]

exampleTable :: Html
exampleTable = bootstrapTable exampleColonnade exampleData

--------------------------------------------------------------------------------

type HomeAPI = Webpage

homeHandlers :: AppServer HomeAPI
homeHandlers = authNotRequired
             $ makePage "Home" (pure Home) $(hamletFile "home")

--------------------------------------------------------------------------------
