module Servant.API.Auth.Token.Internal.Schema(
    schemaOptionsDropPrefix
  ) where 

import Data.Char 
import Data.List
import Data.Swagger

-- | Strip given prefix from fields
schemaOptionsDropPrefix :: String -> SchemaOptions
schemaOptionsDropPrefix pr = defaultSchemaOptions {
    fieldLabelModifier = dropPrefix 
  }
  where 
    dropPrefix s = case stripPrefix pr s of 
      Nothing -> s 
      Just s' -> fixCase s' 

    fixCase x = case x of 
      [] -> []
      (c:cs) -> toLower c : cs