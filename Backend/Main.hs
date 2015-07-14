{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Paths_DailyFeeling_backend

type API = "home" :> Raw

api :: Proxy API
api = Proxy

raw :: FilePath -> Application
raw dir = serveDirectory dir

server :: FilePath -> Server API
server dir = raw dir

app :: FilePath -> Application
app dir = serve api $ server dir


main :: IO ()
main = do
  dir <- getDataFileName "frontend"
  run 8080 $ app dir
