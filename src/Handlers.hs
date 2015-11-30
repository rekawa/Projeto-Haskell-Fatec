{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}

module Handlers where
import Import
import Yesod
import Yesod.Static
import Foundation
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text
import Text.Lucius -- importa arquivo CSS externo
-- import Text.Julius (importa arquivo JS externo)
-- o HTML é importado sem fazer nada


import Database.Persist.Postgresql

-- mkYesodData é para o Foundation
-- mkYesodDispatch é para os Handlers

mkYesodDispatch "Sitio" pRoutes


----------------------------------------------------------------------------------------------------------------------------------
-- Passar parâmetros para a função
widgetForm :: Route Sitio -> Enctype -> Widget -> Text -> Text -> Widget
widgetForm x enctype widget y val = do
     msg <- getMessage
     $(whamletFile "form.hamlet")
-- a linha 27 importa o arquivo nesta parte do código do widget
     toWidget $(luciusFile "teste.lucius")
----------------------------------------------------------------------------------------------------------------------------------
--Criação do formulário para o Usuario
formUsu :: Form Usuario
formUsu = renderDivs $ Usuario <$>
    areq textField FieldSettings{fsId=Just "hident22",
                   fsLabel= "",
                   fsTooltip= Nothing,
                   fsName= Nothing,
                   fsAttrs=[("maxlength","30"),("class","form-control"),("style","max-width:200px;display:block;margin:2px auto"),("placeholder","Login")]} Nothing <*>
    areq passwordField FieldSettings{fsId=Just "hident22",
                   fsLabel= "",
                   fsTooltip= Nothing,
                   fsName= Nothing,
                   fsAttrs=[("class","form-control"),("style","max-width:200px;display:block;margin:2px auto"),("placeholder","Senha")]} Nothing
---------------------------------------------------------------------------------------------------------------------------------
--Chamar formuário de Usuário
getUsuarioR :: Handler Html
getUsuarioR = do
    (wid,enc) <- generateFormPost formUsu
    defaultLayout $ widgetBootstrap >> widgetForm UsuarioR enc wid "Cadastro de Usuarios" "Cadastrar" >> widgetVoltar

----------------------------------------------------------------------------------------------------------------------------------
--Criação do formulário para o Usuario
formMat :: Form Materia
formMat = renderDivs $ Materia <$>
    areq textField FieldSettings{fsId=Just "hident22",
                   fsLabel= "",
                   fsTooltip= Nothing,
                   fsName= Nothing,
                   fsAttrs=[("maxlength","30"),("class","form-control"),("style","max-width:200px;display:block;margin:2px auto"),("placeholder","Materia")]} Nothing

----------------------------------------------------------------------------------------------------------------------------------
-- Mostra Imagem
getImgR :: Handler Html
getImgR = defaultLayout [whamlet| <img src=@{StaticR empolgou_jpg}> |]
----------------------------------------------------------------------------------------------------------------------------------
--Widget do Bootstrap
widgetBootstrap :: Widget
widgetBootstrap = toWidgetHead [hamlet|
<link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css" integrity="sha512-dTfge/zgoMYpP7QbHy4gWMEGsbsdZeCXz7irItjcC3sPUFtf0kuFbDz/ixG7ArTxmDjLXDmezHubeNikyKGVyQ==" crossorigin="anonymous">
<script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/js/bootstrap.min.js" integrity="sha512-K1qjQ+NcF2TYO/eI3M6v8EiNYZfA95pQumfvcVrTHtwQVDG+aHRqLi/ETn2uB+1JqwYqVG3LIvdm9lj6imS/pQ==" crossorigin="anonymous">
|]
----------------------------------------------------------------------------------------------------------------------------------
-- Verifica a session para dar mensagem de boas vindas
getWelcomeR :: Handler Html
getWelcomeR = do
     usr <- lookupSession "_ID"
-- Verifica se a sessão ID existe. lookupsession sempre é um maybe, dentro da monad handler
     defaultLayout (widget1 >> pWidget)
----------------------------------------------------------------------------------------------------------------------------------
-- Chama a página do formuário da Matéria
getMateriaR :: Handler Html
getMateriaR = do
    (wid,enc) <- generateFormPost formMat
    defaultLayout $ widgetBootstrap >> widgetForm MateriaR enc wid "Cadastro de Matérias" "Cadastrar" >> widgetVoltar
----------------------------------------------------------------------------------------------------------------------------------
-- Mostra o formuláriod de classes
getClasseR :: Handler Html
getClasseR = do
           (widget, enctype) <- generateFormPost formClasse
           defaultLayout $ widgetForm ClasseR enctype widget "Classe" "Cadastrar"
----------------------------------------------------------------------------------------------------------------------------------
-- Cadastrar classes
postClasseR :: Handler Html
postClasseR = do
            ((result,_),_) <- runFormPost formClasse
            case result of
                FormSuccess x -> (runDB $ insert x) >> defaultLayout [whamlet|<h1> Classe inserida|] >>
                     redirect ClasseR
                _ -> redirect ClasseR

----------------------------------------------------------------------------------------------------------------------------------
-- Listar Classes
getListarClasseR :: Handler Html
getListarClasseR = do
                 classe <- runDB $ (rawSql "SELECT ??, ??, ?? \
                                   \FROM usuario INNER JOIN classe \
                                   \ON classe.usuario_id=usuario.id \
                                   \INNER JOIN materia \
                                   \ON classe.materia_id=materia.id" [])::Handler [(Entity Classe, Entity Usuario, Entity Materia)]
                 defaultLayout [whamlet|
                      <body style="background-color: #575757;">
                      <h1 style="text-align: center;font-size: 20px;color: #ffffff;font-family: century gothic;"> Lista de Classe
                      $forall (Entity oq _, Entity _ np, Entity _ nm) <- classe
                          <p style="text-align: center;font-size: 14px;color: #ffffff;font-family: century gothic;"> Classe #{fromSqlKey oq}:  #{usuarioNome np} : #{materiaNome nm}
                      <br>
                      <h1 style="text-align: center;font-size: 20px;color: #ffffff;font-family: century gothic;"><a href=@{WelcomeR}>Voltar</a>
                 |]
----------------------------------------------------------------------------------------------------------------------------------
-- Formulário da Classe
formClasse :: Form Classe
formClasse = renderDivs $ Classe <$>
             areq (selectField aluno) "Aluno" Nothing <*>
             areq (selectField materia) "Forn" Nothing

aluno = do
       entidades <- runDB $ selectList [] [Asc UsuarioNome] 
       optionsPairs $ fmap (\ent -> (usuarioNome $ entityVal ent, entityKey ent)) entidades

materia = do
       entidades <- runDB $ selectList [] [Asc MateriaNome] 
       optionsPairs $ fmap (\ent -> (materiaNome $ entityVal ent, entityKey ent)) entidades

----------------------------------------------------------------------------------------------------------------------------------
--Listar Materia
getListarMateriaR :: Handler Html
getListarMateriaR = do
                 materia <- runDB $ selectList [] [Asc MateriaNome]
                 defaultLayout [whamlet|
                      <body style="background-color: #575757;">
                      <h1 style="text-align: center;font-size: 20px;color: #ffffff;font-family: century gothic;"> Lista de Matérias
                      $forall Entity fid fent <- materia
                          <h2 style="text-align: center;font-size: 14px;color: #ffffff;font-family: century gothic;"> #{materiaNome fent}
                      <br>
                      <h1 style="text-align: center;font-size: 20px;color: #ffffff;font-family: century gothic;"><a href=@{WelcomeR}>Voltar</a>
                 |]
----------------------------------------------------------------------------------------------------------------------------------
-- Página para cadastrar Matéria
postMateriaR :: Handler Html
postMateriaR = do
    ((result,_),_) <- runFormPost formMat
    case result of
        FormSuccess usr -> do
            runDB $ insert usr
            setMessage $ [shamlet| <p> Materia inserida com sucesso! |]
            redirect MateriaR
        _ -> redirect MateriaR
----------------------------------------------------------------------------------------------------------------------------------
-- HTML básico da página principal
widget1 :: Widget
widget1 = [whamlet|
<head>
   <title>すべてがHASKELLになる
<body id="body">
        <div id="main">
        <div id="header">
        <div id="grayline">
        <div id="menu">
            <ul class="menu">
                <li><b>HOME</b>
              | <li><a href=@{ListarMateriaR}>MATERIAS</a>
              | <li><a href=@{ListarClasseR}>CLASSES</a>
              | <li><a href=@{AdminR}>ADMIN</a>
              | <li><a href=@{ByeR}>SAIR</a>
        <div id="katana">
        <div id="upperinfo">
        <div id="info1">
            <p class="conteudo">Haskell é uma linguagem de programação puramente funcional, de propósito geral, nomeada em homenagem ao lógico Haskell Curry. Como uma linguagem funcional, a estrutura de controle primária é a função; a linguagem é baseada nas observações de Haskell Curry e seus descendentes intelectuais. Seu último padrão semi-oficial é o Haskell 98, destinado a especificar uma versão mínima e portável da linguagem para o ensino e como base para futuras extensões. Haskell é a linguagem funcional sobre a qual mais se realizam pesquisas atualmente. Muito utilizada no meio acadêmico. É uma linguagem nova, elaborada em 1987, derivada de outras linguagens funcionais como por exemplo Miranda e ML. Ela se baseia em um estilo de programação em que se enfatiza mais o que deve ser feito (what) em detrimento de como deve ser feito (how). É uma linguagem que possui foco no alcance de soluções para problemas matemáticos, clareza, e de fácil manutenção nos código, e possui uma variedade de aplicações e apesar de simples é muito poderosa.
        <div id="info2">
            <p class="conteudo">O conceito de avaliação preguiçosa já estava difundido no meio acadêmico desde o final da década de 1970. Esforços nessa área incluíam técnicas de redução de grafo e a possibilidade de uma mudança radical na arquitetura de von Neumann. Após o lançamento de Miranda em 1985, diversas outras linguagens funcionais de semântica não rígida proliferaram, como Lazy ML, Orwell, Alfl, Id, Clean, Ponder e Daisy (um dialeto de Lisp). Mesmo após dois anos, Miranda ainda era a mais usada, mas não estava em domínio público. Em setembro 1987 foi realizada uma conferência Functional Programming Languages and Computer Architecture (FPCA '87), em Oregon, o consenso foi a criação de um comitê com o objetivo de construir um padrão aberto para tais linguagens. Isso consolidaria as linguagens existentes, servindo como base para pesquisas futuras no desenvolvimento de linguagens. A primeira reunião do comitê foi realizada em janeiro de 1988, e algumas das metas da linguagem foram discutidas. A linguagem deveria ser de fácil ensino, deveria ser completamente descrita através de uma sintaxe e semântica formal, deveria estar disponível livremente. Haskell foi criada da necessidade de unir as outras linguagens do mesmo paradigma em uma só.
        <div id="footer1"><br>
            <h2>Também estamos no twitter e facebook. Assine também o nosso feed de notícias.<br>
            <a href="http://twitter.com"><img src="http://i57.tinypic.com/2l8gjtu.png" width="150px" height="150px">
            <a href="http://facebook.com"><img src="http://i61.tinypic.com/2m41zj7.png" width="150px" height="150px">
            <a href="#"><img src="http://i62.tinypic.com/15s6fpl.png" width="150px" height="150px">
        <div id="footer2">
            <h1>Faculdade de Tecnologia do Estado de São Paulo
            2015 - Desenvolvido por Ana Carolina & Allan Alencar & Yuri Gregorio. Todos os direitos reservados.
|]
----------------------------------------------------------------------------------------------------------------------------------
-- Widget de Voltar

widgetVoltar :: Widget
widgetVoltar = [whamlet|

<h1 style="color:#FFFFFF;align:center;"><a href=@{WelcomeR}>Voltar</a>

|]

----------------------------------------------------------------------------------------------------------------------------------
-- HTML básico da página do Admin
widget2 :: Widget
widget2 = [whamlet|
<head>
   <title>すべてがHASKELLになる
<body id="body">
        <div id="main">
        <div id="header">
        <div id="grayline">
        <div id="menu">
            <ul class="menu">
                <li><b>ADMIN</b>
              | <li style="font-size:12px;"><a href=@{UsuarioR}>CADASTRAR USUARIO</a>
              | <li style="font-size:12px;"><a href=@{ListUserR}>LISTAR USUARIO</a>
              | <li style="font-size:12px;"><a href=@{MateriaR}>CADASTRAR MATERIAS</a>
              | <li style="font-size:12px;"><a href=@{ListarMateriaR}>LISTAR MATERIAS</a>
              | <li style="font-size:12px;"><a href=@{ClasseR}>CADASTRAR CLASSES</a>
              | <li style="font-size:12px;"><a href=@{ListarClasseR}>LISTAR CLASSES</a>
              | <li style="font-size:12px;"><a href=@{ByeR}>SAIR</a>
        <div id="katana">
        <div id="upperinfo">
        <div id="info1">
            <p class="conteudo">Haskell é uma linguagem de programação puramente funcional, de propósito geral, nomeada em homenagem ao lógico Haskell Curry. Como uma linguagem funcional, a estrutura de controle primária é a função; a linguagem é baseada nas observações de Haskell Curry e seus descendentes intelectuais. Seu último padrão semi-oficial é o Haskell 98, destinado a especificar uma versão mínima e portável da linguagem para o ensino e como base para futuras extensões. Haskell é a linguagem funcional sobre a qual mais se realizam pesquisas atualmente. Muito utilizada no meio acadêmico. É uma linguagem nova, elaborada em 1987, derivada de outras linguagens funcionais como por exemplo Miranda e ML. Ela se baseia em um estilo de programação em que se enfatiza mais o que deve ser feito (what) em detrimento de como deve ser feito (how). É uma linguagem que possui foco no alcance de soluções para problemas matemáticos, clareza, e de fácil manutenção nos código, e possui uma variedade de aplicações e apesar de simples é muito poderosa.
        <div id="info2">
            <p class="conteudo">O conceito de avaliação preguiçosa já estava difundido no meio acadêmico desde o final da década de 1970. Esforços nessa área incluíam técnicas de redução de grafo e a possibilidade de uma mudança radical na arquitetura de von Neumann. Após o lançamento de Miranda em 1985, diversas outras linguagens funcionais de semântica não rígida proliferaram, como Lazy ML, Orwell, Alfl, Id, Clean, Ponder e Daisy (um dialeto de Lisp). Mesmo após dois anos, Miranda ainda era a mais usada, mas não estava em domínio público. Em setembro 1987 foi realizada uma conferência Functional Programming Languages and Computer Architecture (FPCA '87), em Oregon, o consenso foi a criação de um comitê com o objetivo de construir um padrão aberto para tais linguagens. Isso consolidaria as linguagens existentes, servindo como base para pesquisas futuras no desenvolvimento de linguagens. A primeira reunião do comitê foi realizada em janeiro de 1988, e algumas das metas da linguagem foram discutidas. A linguagem deveria ser de fácil ensino, deveria ser completamente descrita através de uma sintaxe e semântica formal, deveria estar disponível livremente. Haskell foi criada da necessidade de unir as outras linguagens do mesmo paradigma em uma só.
        <div id="footer1"><br>
            <h2>Também estamos no twitter e facebook. Assine também o nosso feed de notícias.<br>
            <a href="http://twitter.com"><img src="http://i57.tinypic.com/2l8gjtu.png" width="150px" height="150px">
            <a href="http://facebook.com"><img src="http://i61.tinypic.com/2m41zj7.png" width="150px" height="150px">
            <a href="#"><img src="http://i62.tinypic.com/15s6fpl.png" width="150px" height="150px">
        <div id="footer2">
            <h1>Faculdade de Tecnologia do Estado de São Paulo
            2015 - Desenvolvido por Ana Carolina & Allan Alencar & Yuri Gregorio. Todos os direitos reservados.
|]
----------------------------------------------------------------------------------------------------------------------------------
-- CSS da página inicial
pWidget :: Widget
pWidget = do
       [whamlet||]
       toWidget [lucius|
#main{
    background-color: #111111;
}

#header{
    background-color: #FEF91B;
    height: 15px;
    margin:0;

}

#grayline{
    background-color: #535353;
    height: 1px;
}
.menu li{
    display: inline-block;

}

.menu {
        margin: 0;
}

#menu{
    background-color: #151515;
    height: 180px;
    margin:0;
    color: #ffffff;
    font-family: century gothic;
    font-size: 30px;
    text-align: center;
    line-height: 180px;
}

    a:link {color:#ffffff;}      /* unvisited link */
    a:visited {color: #ebebeb;}  /* visited link */
    a:hover {color: #ff9f9f }  /* mouse over link */
    a:active {color:#aeaeae;}  /* selected link */
    a {
        text-decoration: none;
    }


#body{
    margin: 0;
    background-color: #111111;

}
#upperinfo{
    background-color: #575757;
    height: 50px;
    line-height: 50px;
}

#upperinfo2{
    background-color: #221d32;
    height: 50px;
    line-height: 50px;
}
#upperinfo3{
    background-color: #7c6533;
    height: 50px;
    line-height: 50px;
}

#upperinfo4{
    background-color: #464237;
    height: 50px;
    line-height: 50px;
}


#info1{
    background-color: #575757;
    width: 40%;
    height: 500px;
    float: left;
    color: #ffffff;
    font-family: century gothic;
    font-align: justify;
}
#info2{
    background-color: #575757;
    width: 60%;
    height: 500px;
    float: right;
    color: #ffffff;
    font-family: century gothic;
    font-align: justify;
}
#info3{
    width: 100%;
    float: left;
    background-color: #221d32;
    color: #ffffff;
    font-family: century gothic;
    font-align: justify;
}

#info4{
    width: 100%;
    float: left;
    background-color: #464237;
    color: #ffffff;
    font-family: century gothic;
    font-align: justify;
}

#info5{
    width: 100%;
    float: left;
    background-color: #7c6533;
    color: #ffffff;
    font-family: century gothic;
    font-align: justify;
}


#katana{
    height: 600px;
    background-image: url("http://i.imgur.com/M86sRUw.jpg");
}


p{
    margin:0;
}
#upper{
    text-align: right;
    font-family: century gothic;
    font-size: 12px;

}

#footer1{
    height: 300px;
    clear:both;
    background-color: #111111;
    text-align: center;
    color: #d9d9d9;
    font-family: century gothic;

}

#footer2{
    height: 100px;
    clear:both;
    background-color: #111111;
    text-align: center;
    color: #363636;
    font-family: century gothic;

}

h1{
    text-align: center;
    font-size: 20px;
    color: #363636;
    font-family: century gothic;
}

h2{
    font-size: 20px;
    text-align: center;
    color: #d9d9d9;
    font-family: century gothic;
}

.conteudo{
    margin:20px;
    text-align: justify;
}
       |]

----------------------------------------------------------------------------------------------------------------------------------
-- Página de Login
getLoginR :: Handler Html
getLoginR = do
    (wid,enc) <- generateFormPost formUsu
    defaultLayout $ widgetBootstrap >> widgetForm LoginR enc wid "" "Entrar"

postLoginR :: Handler Html
postLoginR = do
    ((result,_),_) <- runFormPost formUsu
    case result of
        FormSuccess usr -> do
            usuario <- runDB $ selectFirst [UsuarioNome ==. usuarioNome usr, UsuarioPass ==. usuarioPass usr ] []
-- comparação no sql pelo haskell é ==. e o comando AND é a vírgula
            case usuario of
                Just (Entity uid usr) -> do
                    setSession "_ID" (usuarioNome usr)
                    redirect WelcomeR
                Nothing -> do
                    setMessage $ [shamlet| Invalid user |]
                    redirect LoginR
        _ -> redirect LoginR
----------------------------------------------------------------------------------------------------------------------------------
-- Página para cadastrar usuário
postUsuarioR :: Handler Html
postUsuarioR = do
    ((result,_),_) <- runFormPost formUsu
    case result of
        FormSuccess usr -> do
            runDB $ insert usr
            setMessage $ [shamlet| <p> Usuario inserido com sucesso! |]
            redirect UsuarioR
        _ -> redirect UsuarioR
----------------------------------------------------------------------------------------------------------------------------------
-- Página para listar usuário
getListUserR :: Handler Html
getListUserR = do
    listaU <- runDB $ selectList [] [Asc UsuarioNome]
    defaultLayout $(whamletFile "list.hamlet")
----------------------------------------------------------------------------------------------------------------------------------
-- Página de logout
getByeR :: Handler Html
getByeR = do
    deleteSession "_ID"
    defaultLayout [whamlet|

    <head>
        <meta http-equiv="refresh" content=1;url=@{LoginR}>
|]
----------------------------------------------------------------------------------------------------------------------------------
-- Página de login do Admin
getAdminR :: Handler Html
getAdminR = defaultLayout (widget2 >> pWidget)
----------------------------------------------------------------------------------------------------------------------------------
-- Conexão com o banco de dados
connStr = "dbname=d508chu1v7g4pb host=ec2-107-21-219-201.compute-1.amazonaws.com user=cfvomntvuoojqt password=1bsark6ZjJyiuBeTFlr_Vwf5CA port=5432"
----------------------------------------------------------------------------------------------------------------------------------
main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool
       s <- static "."
       warpEnv (Sitio pool s)