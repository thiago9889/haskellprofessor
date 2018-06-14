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

        postPerfilCapituloR :: CapituloId -> Handler Html
        postPerfilCapituloR cid = do
            runDB $ delete cid
            redirect ListaCapituloR

        postCadCapituloR :: NivelbId -> Handler Html
        postCadCapituloR nid = do
            ((result,_),_) <- runFormPost formCapitulo
            case result of
                FormSuccess cap -> do
                    cid <- runDB $ insert $ Capitulo(capituloNumero cap) nid
                    redirect (PerfilCapituloR cid)
                _ -> redirect HomeR


        getPerfilCapituloR :: CapituloId -> Handler Html
        getPerfilCapituloR cid = do
            cap <- runDB $ get404 cid
            defaultLayout $ do
                toWidget $(luciusFile "templates/home.lucius")
                $(whamletFile "templates/menu.hamlet")
                [whamlet|
                    <h2>
                        Número: #{capituloNumero cap}
               |]
