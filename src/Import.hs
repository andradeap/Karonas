{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Import where

import Yesod

pRoutes = [parseRoutes|
   / HomeR GET 
   /login LoginR GET POST
   /cadastro CadastrarR GET POST
   /caronas CaronasR GET
   /carona/#CaronaId CaronaR GET
   /criarCarona CriarCaronaR GET POST
|]

