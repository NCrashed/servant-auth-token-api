{-# LANGUAGE TemplateHaskell #-}
module Servant.API.Auth.Token.Pagination(
    PageParam
  , Page
  , PageSizeParam
  , PageSize
  -- * Helpers
  , PagedList(..)
  ) where 

import Data.Aeson.WithField 
import Data.Swagger
import Servant.API.Auth.Token.Internal.DeriveJson 
import Servant.API.Auth.Token.Internal.Schema 
import GHC.Generics 
import Servant.API

-- | Query parameter that carries pagination page number
type PageParam = QueryParam "page" Page 
type Page = Word 

-- | Query parameter that carries pagination page size value
type PageSizeParam = QueryParam "size" PageSize 
type PageSize = Word

-- | Collection of 'a' with attached ids of type 'i' and additional
-- page info.
data PagedList i a = PagedList {
  pagedListItems :: ![WithId i a] -- ^ Payload
, pagedListPages :: !Word -- ^ Count of available pages
} deriving (Generic, Show)
$(deriveJSON (derivePrefix "pagedList") ''PagedList)

instance (ToSchema i, ToSchema a) => ToSchema (PagedList i a) where 
  declareNamedSchema = genericDeclareNamedSchema $
    schemaOptionsDropPrefix "pagedList"