{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Capitulo where

import Import
import Text.Lucius
import Database.Persist.Postgresql

formCapitulo :: Form Capitulo
formCapitulo = renderDivs $ Capitulo
        <$> areq textField "Número: " Nothing
        <*> areq hiddenField "" (Just $ toSqlKey 0)

getCadCapituloR :: NivelbId -> Handler Html
getCadCapituloR nid = do
    (widget,enctype) <- generateFormPost formCapitulo
    defaultLayout $ do
        toWidget $(luciusFile "templates/home.lucius")
        $(whamletFile "templates/menu.hamlet")
        [whamlet|
            <h2>
                Inserir Capítulo
            <form action=@{CadCapituloR nid} method=post enctype=#{enctype}>
                ^{widget}
                <input type="submit" value="OK">
        |]
