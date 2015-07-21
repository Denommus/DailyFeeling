{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Paths_DailyFeeling_backend
import DailyFeeling.Common.Types
import Control.Monad.Trans.Either
import Control.Monad.IO.Class
import Database.Groundhog.Sqlite
import Database.Groundhog.TH

type API = "entries" :> Get '[JSON] [Entry]
           :<|> "entries" :> ReqBody '[JSON] Entry :> Post '[JSON] Entry
           :<|> Raw

mkPersist defaultCodegenConfig [groundhog|
- entity: Mood
- entity: Entry
|]

entries :: Sqlite -> EitherT ServantErr IO [Entry]
entries sql = do
  result <- flip runDbConn sql $ select (NameField /=. ("" :: String))
  return $ result

newEntry :: Sqlite -> Entry -> EitherT ServantErr IO Entry
newEntry sql new = do
  flip runDbConn sql $ insert new
  return new

-- entry :: Integer -> Maybe Entry
-- entry i = find ((==i) . entryId) entries

api :: Proxy API
api = Proxy

raw :: FilePath -> Application
raw dir = serveDirectory dir

server :: FilePath -> Sqlite -> Server API
server dir sql = entries sql
                 :<|> newEntry sql
                 :<|> raw dir

app :: FilePath -> Sqlite -> Application
app dir sql = serve api $ server dir sql


main :: IO ()
main = do
  dir <- getDataFileName "frontend"
  withSqliteConn ":memory:" $ \sql -> do
    flip runDbConn sql $ runMigration $ do
      -- migrate (undefined :: Mood)
      migrate (undefined :: Entry)
    liftIO . run 8080 $ app dir sql
