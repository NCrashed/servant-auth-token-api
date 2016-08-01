module Main where

import Servant.API.Auth.Token 
import Servant.Swagger
import System.Environment
import Data.Aeson
import Data.Proxy

import qualified Data.ByteString.Lazy as BS 

main :: IO ()
main = do
  [targetFile] <- getArgs
  BS.writeFile targetFile $ encode $ toSwagger (Proxy :: Proxy AuthAPI)