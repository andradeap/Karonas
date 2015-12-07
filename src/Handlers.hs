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
import Text.Lucius

import Database.Persist.Postgresql

mkYesodDispatch "Karonas" pRoutes

widgetForm :: Route Karonas -> Enctype -> Widget -> Text -> Text -> Widget
widgetForm x enctype widget y val = do
     msg <- getMessage
     $(whamletFile "forms.hamlet")
     toWidget $(luciusFile "css.lucius")

widgetGeral :: Route Karonas -> Enctype -> Widget -> Text -> Text -> Widget
widgetGeral x enctype widget y val = do
     msg <- getMessage
     $(whamletFile "template.hamlet")
     toWidget $(luciusFile "css.lucius")     

formLogin :: Form Usuario
formLogin = renderDivs $ Usuario <$>
            areq textField "Usuario" Nothing <*>
            areq passwordField "Senha" Nothing

formUsuario :: Form Usuario
formUsuario = renderDivs $ Usuario <$>
              areq textField "Usuario" Nothing <*>
              areq passwordField FieldSettings{fsId= Just "teste",
                             fsLabel="Senha",
                             fsTooltip= Nothing,
                             fsName= Nothing,
                             fsAttrs=[("minlength","8")]} Nothing

formCarona :: Form Carona
formCarona = renderDivs $ Carona <$>
             areq textField"Origem" Nothing <*>
             areq textField "Destino" Nothing <*>
             areq doubleField "Valor" Nothing <*>
             areq intField "Numero de Passageiros" Nothing <*>
             areq textField "Motorista" Nothing


getHomeR :: Handler Html
getHomeR = do
     usr <- lookupSession "_ID"
     defaultLayout [whamlet|
        $maybe m <- usr
            <h2> Welcome #{m}
     
     |]

getLoginR ::Handler Html
getLoginR = do
        (wid,enc) <- generateFormPost formLogin
        defaultLayout $ widgetForm LoginR enc wid "Entre" "Login"

postLoginR :: Handler Html
postLoginR = do
    ((result,_),_) <- runFormPost formUsuario
    case result of
        FormSuccess usr -> do
            usuario <- runDB $ selectFirst [UsuarioLogin ==. usuarioLogin usr, UsuarioSenha ==. usuarioSenha usr ] []
            case usuario of
                Just (Entity uid usr) -> do
                    setSession "_ID" (usuarioLogin usr)
                    redirect HomeR
                Nothing -> do
                    setMessage $ [shamlet| Usuario invalido! Tente mais uma vez ou cadastre-se. |]
                    redirect LoginR
        _ -> redirect LoginR

getCadastrarR :: Handler Html
getCadastrarR = do
             (widget, enctype) <- generateFormPost formUsuario
             defaultLayout $ widgetForm CadastrarR enctype widget "Cadastre-se" "Cadastrar Usuario"

postCadastrarR :: Handler Html
postCadastrarR = do
                ((result, _), _) <- runFormPost formUsuario
                case result of
                    FormSuccess usuario -> do
                       runDB $ insert usuario
                       defaultLayout [whamlet|
                           <h1> Cadastrado com sucesso
                       |]
                    _ -> redirect CadastrarR

getCaronasR :: Handler Html
getCaronasR = do
             listaC <- runDB $ selectList [] [Asc CaronaOrigem]
             defaultLayout [whamlet|
                 <h1> Caronas cadastradas:
                 $forall Entity cid carona <- listaC
                     <a href=@{CaronaR cid}> #{caronaOrigem carona} <br>
             |]

getCaronaR :: CaronaId -> Handler Html
getCaronaR cid = do
             carona <- runDB $ get404 cid
             defaultLayout [whamlet|
                 <h1> Carona #{fromSqlKey cid}
                 <p> Origem: #{caronaOrigem carona}
                 <p> Destino:#{caronaDestino carona}
                 <p> Numero de Passageiros Disponiveis: #{caronaPassageiros carona}
                 <p> Valor: #{caronaValor carona}
                 <p> Motorista: #{caronaMotorista carona}
             |]

getCriarCaronaR :: Handler Html
getCriarCaronaR = do
        (wid,enc) <- generateFormPost formCarona
        defaultLayout $ widgetForm CriarCaronaR enc wid "" "Criar Carona"

postCriarCaronaR :: Handler Html
postCriarCaronaR = do
                ((result,_),_) <- runFormPost formCarona
                case result of
                    FormSuccess carona -> (runDB $ insert carona) >> defaultLayout [whamlet|<h1> Carona criada|]
                    _ -> redirect CriarCaronaR
  -- /addLocal AddLocalR GET POST

getSairR :: Handler Html
getSairR = do
    deleteSession "_ID"
    defaultLayout [whamlet| BYE! |]

connStr = "dbname=d2ch1h63l97e6f host=ec2-54-204-40-209.compute-1.amazonaws.com user=bqyouiefbhxcuv password=AAvwA5dG-B2w3s2-X4OCWpDz-x port=5432"

main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool
       s <- static "."
       warpEnv (Karonas pool s)