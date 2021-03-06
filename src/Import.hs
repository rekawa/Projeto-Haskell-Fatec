{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Import where

import Yesod
import Yesod.Static
-- o import da linha 5 é importante para colocar imagem interna.

-- o da linha 15 é o acessor de todas as imagens (tudo o que for estático)
-- Static é um método diferente que recebe a rota, getStatic recebe a função da imagem
-- exemplo: @{StaticR empolgou_jpg}

pRoutes = [parseRoutes|
   /user UsuarioR GET POST
   /listar ListUserR GET
   /static StaticR Static getStatic
   /ima ImgR GET
   /login LoginR GET POST
   / WelcomeR GET
   /bye ByeR GET
   /admin AdminR GET
   /materia MateriaR GET POST
   /classe ClasseR GET POST
   /listarclasse ListarClasseR GET
   /listarmateria ListarMateriaR GET
|]