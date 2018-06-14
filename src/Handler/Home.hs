{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Home where

import Import
import Text.Lucius

import Database.Persist.Postgresql(toSqlKey)

getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        -- addStylesheet $ StaticR css_bootstrap_css
        toWidget $(luciusFile "templates/home.lucius")
        $(whamletFile "templates/menu.hamlet")
        $(whamletFile "templates/home.hamlet")
