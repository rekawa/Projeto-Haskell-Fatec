{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleContexts,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns #-}
module Foundation where
import Import
import Yesod
import Yesod.Static
import Data.Text
import Database.Persist.Postgresql
    ( ConnectionPool, SqlBackend, runSqlPool, runMigration )

data Sitio = Sitio { connPool :: ConnectionPool,
                     getStatic :: Static }

staticFiles "."
-- esse ponto é a pasta local (linux), se fosse uma pasta externa, colocar o nome da pasta no staticFiles.

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Usuario
   nome Text
   pass Text
   deriving Show

Materia
    nome Text

Classe
    usuarioId UsuarioId
    materiaId MateriaId
    UniqueUsuarioMateria usuarioId materiaId

|]

mkYesodData "Sitio" pRoutes

instance YesodPersist Sitio where
   type YesodPersistBackend Sitio = SqlBackend
   runDB f = do
       master <- getYesod
       let pool = connPool master
       runSqlPool f pool

-- autoriza a entrada
instance Yesod Sitio where
    authRoute _ = Just $ LoginR
    isAuthorized LoginR _ = return Authorized
    isAuthorized AdminR _ = isAdmin
    isAuthorized UsuarioR _ = return Authorized
    isAuthorized _ _ = isUser

-- verifica a sessão e as autorizações
isUser = do
    mu <- lookupSession "_ID"
    return $ case mu of
        Nothing -> AuthenticationRequired
        Just _ -> Authorized

isAdmin = do
    mu <- lookupSession "_ID"
    return $ case mu of
        Nothing -> AuthenticationRequired
        Just "admin" -> Authorized
        Just _ -> Unauthorized "Acesso negado. Apenas o administrador pode acessar esta página."

type Form a = Html -> MForm Handler (FormResult a, Widget)

instance RenderMessage Sitio FormMessage where
    renderMessage _ _ = defaultFormMessage
