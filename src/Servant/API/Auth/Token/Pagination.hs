{-# LANGUAGE TemplateHaskell #-}
{-|
Module      : Servant.API.Auth.Token.Pagination
Description : Helpers for response pagination support.
Copyright   : (c) Anton Gushcha, 2016
License     : MIT
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : Portable
-}
module Servant.API.Auth.Token.Pagination(
  -- * API types
    PageParam
  , Page
  , PageSizeParam
  , PageSize
  -- ** Helpers
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
-- | A page number
type Page = Word 

-- | Query parameter that carries pagination page size value
type PageSizeParam = QueryParam "size" PageSize 
-- | Number of items on a page
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