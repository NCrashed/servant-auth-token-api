module Servant.API.Auth.Token.Internal.DeriveJson(
    derivePrefix
  , deriveJSON
  ) where 

import Data.Aeson.TH               as TH
import Data.Char                   as C 
import Data.Text (Text)

import qualified Data.Text         as T

-- | For aeson deriving, drop prefix t and map to lower
derivePrefix :: Text -> TH.Options
derivePrefix t = defaultOptions {
    fieldLabelModifier = mapFirst C.toLower . drop (T.length t) 
  , constructorTagModifier = camelCaseToDash
  }

mapFirst :: (a -> a) -> [a] -> [a]
mapFirst f cs = case cs of 
  [] -> []
  (x:xs) -> f x : xs  

camelCaseToDash :: String -> String
camelCaseToDash = foldr go "" . mapFirst C.toLower
  where 
  go c acc = if C.isUpper c 
    then '_' : C.toLower c : acc
    else c : acc 