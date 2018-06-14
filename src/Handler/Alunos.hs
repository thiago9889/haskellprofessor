{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Alunos where

import Import
import Text.Lucius
import Database.Persist.Postgresql

formAlunos :: Form (Text, Maybe FileInfo, Key Capitulo) 
formAlunos = renderDivs $ (,,)
        <$> areq textField "Nome: " Nothing
        <*> aopt fileField FieldSettings{fsId=Just "hident4",
                                fsLabel="Arquivo: ",
                                fsTooltip= Nothing,
                                fsName= Nothing,
                                fsAttrs=[("accept","image/jpeg")]} 
                                Nothing
        <*> areq hiddenField "" (Just $ toSqlKey 1)

getAlunosR :: CapituloId -> Handler Html
getAlunosR cid = do 
    (widget,enctype) <- generateFormPost formAlunos
    defaultLayout $ do
        toWidget $(luciusFile "templates/home.lucius")
        $(whamletFile "templates/menu.hamlet")
        [whamlet|
            <h2>
                Inserir Alunos
            <form action=@{AlunosR cid} method=post enctype=#{enctype}>
                ^{widget}
                <br>
                <br>
                <input type="submit" value="OK">
        |]

postAlunosR :: CapituloId -> Handler Html
postAlunosR cid = do 
    -- LEIO OS PARAMETROS DO FORM
    ((res,_),_) <- runFormPost formAlunos
    case res of
        FormSuccess (nome,Just arq, cid) -> do 
            aid <- runDB $ insert $ Alunos nome (Just $ (fileName arq)) cid
            liftIO $ fileMove arq ("static/" ++ (unpack $ fileName arq))
            redirect (PerfilR aid)
        FormSuccess (nome,Nothing, cid) -> do 
            aid <- runDB $ insert $ Alunos nome Nothing cid
            redirect (PerfilR aid)
        _ -> redirect HomeR

getPerfilR :: AlunosId -> Handler Html
getPerfilR aid = do 
    alunos <- runDB $ get404 aid
    imagem <- return $ alunosImagem alunos
    staticDir <- return $ "../../static/"
    defaultLayout $ do 
        toWidget $(luciusFile "templates/home.lucius")
        $(whamletFile "templates/menu.hamlet")
        [whamlet|
            <h2>
                Nome: #{alunosNome alunos}
            <p>
                $maybe img <- imagem 
                    <img src=#{staticDir ++ img}>

        |]
