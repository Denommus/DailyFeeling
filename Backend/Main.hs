{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Paths_DailyFeeling_backend
import DailyFeeling.Common.Types
import Data.List
import Control.Monad.Trans.Either

type API = "entries" :> Get '[JSON] [Entry]
           :<|> "entries" :> Capture "i" Integer :> Get '[JSON] Entry
           :<|> Raw

entries :: [Entry]
entries = [ Entry 1 Happy "Yuri" "Good day"
          , Entry 2 Sad "" "Meh" ]

entry :: Integer -> Maybe Entry
entry i = find ((==i) . entryId) entries

api :: Proxy API
api = Proxy

raw :: FilePath -> Application
raw dir = serveDirectory dir

server :: FilePath -> Server API
server dir = return entries
             :<|> (maybe (left $ err404 { errBody = "not found" }) return) . entry
             :<|> raw dir

app :: FilePath -> Application
app dir = serve api $ server dir


main :: IO ()
main = do
  dir <- getDataFileName "frontend"
  run 8080 $ app dir
