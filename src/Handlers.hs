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

--mkYesodDispatch "Sitio" pRoutes

widgetForm :: Route Sitio -> Enctype -> Widget -> Text -> Text -> Widget
widgetForm x enctype widget y val = do
     msg <- getMessage
     $(whamletFile "form.hamlet")
-- a linha 27 importa o arquivo nesta parte do código do widget
     toWidget $(luciusFile "teste.lucius")

formUsu :: Form Usuario
formUsu = renderDivs $ Usuario <$>
    areq textField "Username" Nothing <*>
    areq textField "Pass" Nothing

getUsuarioR :: Handler Html
getUsuarioR = do
    (wid,enc) <- generateFormPost formUsu
    defaultLayout $ widgetForm UsuarioR enc wid "Cadastro de Usuarios" "Cadastrar"


-------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------
-- verifica a session para dar mensagem de boas vindas
getWelcomeR :: Handler Html
getWelcomeR = do
     usr <- lookupSession "_ID"
-- verifica se a sessão ID existe. lookupsession sempre é um maybe, dentro da monad handler
     defaultLayout [whamlet|
        $maybe m <- usr
            <h1> Welcome #{m}
     |]
--------------------------------------------------------------------------------------
getLoginR :: Handler Html
getLoginR = do
    (wid,enc) <- generateFormPost formUsu
    defaultLayout $ widgetForm LoginR enc wid "" "Log in"

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

postUsuarioR :: Handler Html
postUsuarioR = do
    ((result,_),_) <- runFormPost formUsu
    case result of
        FormSuccess usr -> do
            runDB $ insert usr
            setMessage $ [shamlet| <p> Usuario inserido com sucesso! |]
            redirect UsuarioR
        _ -> redirect UsuarioR

getListUserR :: Handler Html
getListUserR = do
    listaU <- runDB $ selectList [] [Asc UsuarioNome]
    defaultLayout $(whamletFile "list.hamlet")

getByeR :: Handler Html
getByeR = do
    deleteSession "_ID"
    defaultLayout [whamlet| BYE! |]

getAdminR :: Handler Html
getAdminR = defaultLayout [whamlet| <h1> Bem-vindo Admin!!|]

connStr = "dbname=d508chu1v7g4pb host=ec2-107-21-219-201.compute-1.amazonaws.com user=cfvomntvuoojqt password=1bsark6ZjJyiuBeTFlr_Vwf5CA port=5432"

main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool
       --s <- static "."
       --warpEnv (Sitio pool s)
