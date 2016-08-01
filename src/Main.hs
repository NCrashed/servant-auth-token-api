{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Lens
import Data.Aeson
import Data.Proxy
import Data.Swagger 
import Servant.API.Auth.Token 
import Servant.Swagger
import System.Environment

import qualified Data.ByteString.Lazy as BS 

main :: IO ()
main = do
  [targetFile] <- getArgs
  BS.writeFile targetFile $ encode $ toSwagger (Proxy :: Proxy AuthAPI)
    & info.title        .~ "API servant-auth-token-api"
    & info.version      .~ "0.1"
    & info.description  ?~ "This is an API for token based authorisation"
    & info.license      ?~ "MIT"
    & host              ?~ "ncrashed.github.io"
    & applyTagsFor authOperations ["Authorisation" & description ?~ "Authorisation operations"]