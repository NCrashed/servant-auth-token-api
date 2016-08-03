{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
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
import Data.Monoid
import Data.Proxy 
import Data.Swagger
import Data.Swagger.Internal.Schema
import Data.Text (pack)
import Data.Typeable
import GHC.Generics 
import Servant.API
import Servant.API.Auth.Token.Internal.DeriveJson 
import Servant.API.Auth.Token.Internal.Schema 
import Servant.Docs 

-- | Query parameter that carries pagination page number
type PageParam = QueryParam "page" Page 
-- | A page number
type Page = Word 

-- | Query parameter that carries pagination page size value
type PageSizeParam = QueryParam "size" PageSize 
-- | Number of items on a page
type PageSize = Word

instance ToParam PageParam where
  toParam _ = DocQueryParam "page" ["0", "42"] "Index of page" Normal

instance ToParam PageSizeParam where
  toParam _ = DocQueryParam "size" ["42", "10"] "Number of elements on page" Normal

-- | Collection of 'a' with attached ids of type 'i' and additional
-- page info.
data PagedList i a = PagedList {
  pagedListItems :: ![WithId i a] -- ^ Payload
, pagedListPages :: !Word -- ^ Count of available pages
} deriving (Generic, Show)
$(deriveJSON (derivePrefix "pagedList") ''PagedList)

instance (Typeable i, Typeable a, ToSchema i, ToSchema a) => ToSchema (PagedList i a) where 
  declareNamedSchema p = do
    s <- genericDeclareNamedSchema (schemaOptionsDropPrefix "pagedList") p
    return $ rename nm s
    where 
    nm = Just $ "PagedList " <> iname <> " " <> aname
    iname = pack . show $ typeRep (Proxy :: Proxy i)
    aname = pack . show $ typeRep (Proxy :: Proxy a)

instance (ToSample i, ToSample a) => ToSample (PagedList i a) where
  toSamples _ = samples $ [s $ toSamples (Proxy :: Proxy (WithId i a))]
    where 
    s as = PagedList {
        pagedListItems = snd <$> as
      , pagedListPages = 1
      }