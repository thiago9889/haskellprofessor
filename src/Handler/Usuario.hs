{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Usuario where

import Import
import Text.Lucius
import Database.Persist.Postgresql

formUsuario :: Form Usuario
formUsuario = renderDivs $ Usuario
        <$> areq textField "Nome: " Nothing
        <*> areq emailField "Email: " Nothing
        <*> areq passwordField  "Senha: " Nothing

getCadUsuarioR :: Handler Html
getCadUsuarioR = do
    (widget,enctype) <- generateFormPost formUsuario
    defaultLayout $ do
        toWidget $(luciusFile "templates/home.lucius")
        $(whamletFile "templates/menu.hamlet")
        [whamlet|
            <h2>
                Inserir Professor
            <form action=@{CadUsuarioR} method=post enctype=#{enctype}>
                ^{widget}
                <br>
                <br>
                <input type="submit" value="OK">
        |]

        postPerfilUsuarioR :: UsuarioId -> Handler Html
        postPerfilUsuarioR uid = do
            runDB $ delete uid
            redirect ListaUsuarioR

        postCadUsuarioR :: Handler Html
        postCadUsuarioR = do
            ((result,_),_) <- runFormPost formUsuario
            case result of
                FormSuccess usu -> do
                    uid <- runDB $ insert usu
                    redirect (PerfilUsuarioR uid)
                _ -> redirect HomeR


        getPerfilUsuarioR :: UsuarioId -> Handler Html
        getPerfilUsuarioR uid = do
            usu <- runDB $ get404 uid
            defaultLayout $ do
                toWidget $(luciusFile "templates/home.lucius")
                $(whamletFile "templates/menu.hamlet")
                [whamlet|
                    <h2>
                        Nome: #{usuarioNome usu}

                    <p>
                        E-mail: #{usuarioEmail usu}
                |]
