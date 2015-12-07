{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleContexts,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns #-}
module Foundation where
import Import
import Yesod
import Data.Text (Text)
import Data.Time
import Yesod.Static
import Data.Text
import Database.Persist.Postgresql
    ( ConnectionPool, SqlBackend, runSqlPool, runMigration )

data Karonas = Karonas { connPool :: ConnectionPool,
                     getStatic :: Static }

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Usuario
   login Text
   senha Text
   deriving Show

Carona
   origem Text
   destino Text
   valor Double
   passageiros Int
   motorista Text
   deriving Show


|]

mkYesodData "Karonas" pRoutes

instance YesodPersist Karonas where
   type YesodPersistBackend Karonas = SqlBackend
   runDB f = do
       master <- getYesod
       let pool = connPool master
       runSqlPool f pool

instance Yesod Karonas where
    authRoute _ = Just $ LoginR
    isAuthorized LoginR _ = return Authorized
    isAuthorized CadastrarR _ = return Authorized
    isAuthorized HomeR _ = return Authorized
    isAuthorized _ _ = isUser

isUser = do
    mu <- lookupSession "_ID"
    return $ case mu of
        Nothing -> AuthenticationRequired
        Just _ -> Authorized

type Form a = Html -> MForm Handler (FormResult a, Widget)

instance RenderMessage Karonas FormMessage where
    renderMessage _ _ = defaultFormMessage