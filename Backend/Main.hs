{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Paths_DailyFeeling_backend
import DailyFeeling.Common.Types

type API = "entries" :> Get '[JSON] [Entry]
           :<|> Raw

entries :: [Entry]
entries = [Entry Happy "Yuri" "Good day"]

api :: Proxy API
api = Proxy

raw :: FilePath -> Application
raw dir = serveDirectory dir

server :: FilePath -> Server API
server dir = return entries
             :<|> raw dir

app :: FilePath -> Application
app dir = serve api $ server dir


main :: IO ()
main = do
  dir <- getDataFileName "frontend"
  run 8080 $ app dir
