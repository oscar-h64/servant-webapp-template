# Template for Servant Webapps

***WORK IN PROGRESS***

This repository contains a template Haskell web application application which is powered by Servant, and uses:
- [Persistent](https://hackage.haskell.org/package/persistent) for the database (using PostgreSQL)
- [The Shakesperian templates](https://hackage.haskell.org/package/shakespeare) for the pages
- [Servant-Auth](https://hackage.haskell.org/package/servant-auth) for authentication [_NOT COMPLETE_]

It is designed to be easily customisable, so you can add custom fields to the configuration file, or fields to the environment with simple changes to `App.Types.Config`/`App.Types.Environment` and `Main.hs`. The default layout is a simple page using [bootstrap](https://getbootstrap.com/) with a reactive navigation bar, which can be easily changed by editing `templates/base/layout.hamlet` and `templates/components/*`.

## Getting started

1. Generate the JWT key with

        $ stack repl
        > import Servant.Auth.Server
        > writeKey "config/jwt.pem"

    and generate the SSL certificates with 
        
        openssl req -x509 -newkey rsa:4096 -keyout config/key.pem -out config/cert.pem -days 365 -nodes
    
    Then copy `config/config.example.yaml` to `config/config.yaml` and update the settings as appropriate. <!-- and setup Auth --> 

1. Change the app name in the navigation bar text in `templates/base/layout.hamlet`
1. Optionally rename the `AppHandler` and `AppServer` types to suit your app
1. Add your database definitions to `App.Types.Database`
1. Update `App.Pages.Home` and `templates/home.hamlet` for your homepage
1. Add pages (see [Creating Pages](#Creating-Pages))

## Running
Run the app with `stack run`. There are 2 options that can be passed:
- `--config FILE`: Specifies the location of the configuration file (defaults to `config/config.yaml`)
- `--migrate`: If this is specified the app will attempt to do the required database migrations when it starts

## Creating Pages
1. Add a new constructor for the new page to `Page` in `App.Types.Routing`
1. Add a new line to `pageData` with the settings for the new page
1. Add a new module for this page (unless you want to use an existing module). Make sure the `_API` type and the `_Handlers` function are defined, and add them to the `AppAPI` type and `appHandlers` function in `App` respectively. The easiest way to do this is usually copying an existing module
1. Create a hamlet file under `templates/` for this page, and put the main content in there

## TODO:

**Auth:**
- Everything

**Documentation:**
- Most things

**Page building:**
- Add more form elements
- Make forms (and pages in general) accessible
- Add helper for tables

**General:**
- Rearrange some modules - in particular:
    - `App.Types.Routing` is not just types
- Potentially add a re-export module to simplify imports
- Email support (documenation)
- Logging
