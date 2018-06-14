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


getPerfilNivelbR :: NivelbId -> Handler Html
getPerfilNivelbR nid = do 
    niv <- runDB $ get404 nid
    defaultLayout $ do 
        toWidget $(luciusFile "templates/home.lucius")
        $(whamletFile "templates/menu.hamlet")
        [whamlet|
            <h2> 
                Nome: #{nivelbNome niv}
        |]

getListaNivelbR :: Handler Html
getListaNivelbR = do
    niveis <- runDB $ selectList [] [Asc NivelbNome]
    defaultLayout $ do 
        toWidget $(luciusFile "templates/home.lucius")
        $(whamletFile "templates/menu.hamlet")
        [whamlet|
            <h2>
                Lista de Níveis
            <table>
                <thead>
                    <tr>
                        <th>
                            Nome
                        
                        <th>

                        <th>

                        <th>
                            Listar Capítulo do Nível
                            
                            
                
                <tbody>
                    $forall (Entity nid nivelb) <- niveis
                        <tr>
                            <td>
                                <a href=@{PerfilNivelbR nid}> 
                                    #{nivelbNome nivelb}
                            <td>
                                <form action=@{PerfilNivelbR nid} method=post>
                                    <input type="submit" value="Apagar">
                            <td>
                                <a href=@{CadCapituloR nid} >
                                    Cadastrar Capitulo
                            <td>
                                <a href=@{ListaCapitulonivelbR nid} >
                                    Mostrar Capitulos
        |]