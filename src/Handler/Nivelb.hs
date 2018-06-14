{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Nivelb where

import Import 
import Text.Lucius
import Database.Persist.Postgresql

formNivelb :: Form Nivelb 
formNivelb = renderDivs $ Nivelb 
        <$> areq textField "Nome: " Nothing

getCadNivelbR :: Handler Html
getCadNivelbR = do 
    (widget,enctype) <- generateFormPost formNivelb
    defaultLayout $ do 
        toWidget $(luciusFile "templates/home.lucius")
        $(whamletFile "templates/menu.hamlet")
        [whamlet|
            <h2>
                Inserir Nível
            <form action=@{CadNivelbR} method=post enctype=#{enctype}>
                ^{widget}
                <br>
                <br>
                <input type="submit" value="OK">
        |]

postPerfilNivelbR :: NivelbId -> Handler Html
postPerfilNivelbR nid = do 
    runDB $ delete nid 
    redirect ListaNivelbR

postCadNivelbR :: Handler Html
postCadNivelbR = do 
    ((result,_),_) <- runFormPost formNivelb
    case result of
        FormSuccess niv -> do 
            nid <- runDB $ insert niv 
            redirect (PerfilNivelbR nid)
        _ -> redirect HomeR
