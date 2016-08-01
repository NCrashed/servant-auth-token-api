{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-|
Module      : Servant.API.Auth.Token
Description : API for token based authorisation.
Copyright   : (c) Anton Gushcha, 2016
License     : MIT
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : Portable
-}
module Servant.API.Auth.Token(
  -- * API specs
    AuthAPI
  , authAPI
  , UserId
  , SimpleToken
  , Token(..)
  , MToken
  , Login
  , Password
  , Email
  , Permission
  , Seconds
  , RestoreCode
  , TokenHeader
  , ReqRegister(..)
  , RespUserInfo(..)
  , PatchUser(..)
  , RespUsersInfo(..)
  -- ** User groups
  , UserGroupId
  , UserGroup(..)
  , PatchUserGroup(..)
  , UserGroups(..)
  -- ** Default permissions
  , adminPerm
  , registerPerm
  , authInfoPerm
  , authUpdatePerm
  , authDeletePerm
  -- * Swagger helpers
  , authOperations
  ) where 

import Control.Lens
import Data.Aeson.WithField  
import Data.Proxy
import Data.Swagger (Swagger, Operation)
import Data.Swagger.Internal (SwaggerType(..), _paramSchemaType)
import Data.Swagger.Internal.ParamSchema
import Data.Swagger.Internal.Schema
import Data.Swagger.Operation
import GHC.Generics 
import GHC.TypeLits
import Servant.API
import Servant.Swagger

import Data.Text (Text)

import Servant.API.Auth.Token.Pagination
import Servant.API.Auth.Token.Internal.DeriveJson 
import Servant.API.Auth.Token.Internal.Schema

-- | Token is simple string marked by permissions that are expected
-- from the token to pass guarding functions.
newtype Token (perms :: [Symbol]) = Token { unToken :: Text }
  deriving (Eq, Show)

instance ToParamSchema (Token perms) where 
  toParamSchema _ = mempty { _paramSchemaType = SwaggerString }

instance FromHttpApiData (Token perms) where 
  parseUrlPiece = fmap Token . parseUrlPiece

instance ToHttpApiData (Token perms) where 
  toUrlPiece = toUrlPiece . unToken

-- | Token that doesn't have attached compile-time permissions
type SimpleToken = Text 
-- | Shortcut for Maybe Token with attached permissions
type MToken (perms :: [Symbol]) = Maybe (Token perms)

type Login = Text -- ^ Username for login
type Password = Text -- ^ Password for login
type Email = Text -- ^ User email
type Permission = Text -- ^ Special tag for a permission that a user has
type Seconds = Word -- ^ Amount of seconds
type RestoreCode = Text -- ^ Special tag for password restore 

-- | Token header that we require for authorization marked 
-- by permissions that are expected from the token to pass guarding functions.
type TokenHeader (perms :: [Symbol]) = 
  Header "Authorization" (Token perms)

-- | Id of user that is used in the API
type UserId = Word 

-- | Id of user group
type UserGroupId = Word 

-- | Request body for user registration
data ReqRegister = ReqRegister {
  reqRegLogin :: !Login 
, reqRegPassword :: !Password 
, reqRegEmail :: !Email 
, reqRegPermissions :: ![Permission]
, reqRegGroups :: !(Maybe [UserGroupId])
} deriving (Generic, Show)
$(deriveJSON (derivePrefix "reqReg") ''ReqRegister)

instance ToSchema ReqRegister where 
  declareNamedSchema = genericDeclareNamedSchema $
    schemaOptionsDropPrefix "reqReg"

-- | Response with user info
data RespUserInfo = RespUserInfo {
  respUserId :: !Word
, respUserLogin :: !Login 
, respUserEmail :: !Email 
, respUserPermissions :: ![Permission]
, respUserGroups :: ![UserGroupId]
} deriving (Generic, Show)
$(deriveJSON (derivePrefix "respUser") ''RespUserInfo)

instance ToSchema RespUserInfo where 
  declareNamedSchema = genericDeclareNamedSchema $
    schemaOptionsDropPrefix "respUser"

-- | Response with users info and pagination
data RespUsersInfo = RespUsersInfo {
  respUsersItems :: ![RespUserInfo]
, respUsersPages :: !Word 
} deriving (Generic, Show)
$(deriveJSON (derivePrefix "respUsers") ''RespUsersInfo)

instance ToSchema RespUsersInfo where 
  declareNamedSchema = genericDeclareNamedSchema $
    schemaOptionsDropPrefix "respUsers"

-- | Request body for patching user
data PatchUser = PatchUser {
  patchUserLogin :: !(Maybe Login)
, patchUserPassword :: !(Maybe Password)
, patchUserEmail :: !(Maybe Email)
, patchUserPermissions :: !(Maybe [Permission])
, patchUserGroups :: !(Maybe [UserGroupId])
} deriving (Generic, Show)
$(deriveJSON (derivePrefix "patchUser") ''PatchUser)

instance ToSchema PatchUser where 
  declareNamedSchema = genericDeclareNamedSchema $
    schemaOptionsDropPrefix "patchUser"


-- | Data of user group, groups allows to group permissions
-- and assign them to particular users in batch manner.
--
-- Also a group hierarchy can be formed.
data UserGroup = UserGroup {
  userGroupName :: !Text 
, userGroupUsers :: ![UserId]
, userGroupPermissions :: ![Permission]
, userGroupParent :: !(Maybe UserGroupId)
} deriving (Generic, Show)
$(deriveJSON (derivePrefix "userGroup") ''UserGroup)

instance ToSchema UserGroup where 
  declareNamedSchema = genericDeclareNamedSchema $
    schemaOptionsDropPrefix "userGroup"

-- | Data type that is used to patch 'UserGroup'
data PatchUserGroup = PatchUserGroup {
  patchUserGroupName :: !(Maybe Text)
, patchUserGroupUsers :: !(Maybe [UserId])
, patchUserGroupPermissions :: !(Maybe [Permission])
, patchUserGroupParent :: !(Maybe UserGroupId)
-- | Special case when you want to set parent to 'Nothing'
, patchUserGroupNoParent :: !(Maybe Bool)
} deriving (Generic, Show)
$(deriveJSON (derivePrefix "patchUserGroup") ''PatchUserGroup)

instance ToSchema PatchUserGroup where 
  declareNamedSchema = genericDeclareNamedSchema $
    schemaOptionsDropPrefix "patchUserGroup"

-- | Response with part of all known user groups
data UserGroups = UserGroups {
  userGroupsItems :: [WithId UserGroupId UserGroup]
, userGroupsPages :: !Word
} deriving (Generic, Show)
$(deriveJSON (derivePrefix "userGroups") ''UserGroups)

instance ToSchema UserGroups where 
  declareNamedSchema = genericDeclareNamedSchema $
    schemaOptionsDropPrefix "userGroups"

-- | Generic authorization API
type AuthAPI = 
  -- How to get a token, expire of 'Nothing' means 
  -- some default value (server config)
       "auth" :> "signin"
              :> QueryParam "login" Login 
              :> QueryParam "password" Password 
              :> QueryParam "expire" Seconds
              :> Get '[JSON] (OnlyField "token" SimpleToken)
  -- Client cat expand the token lifetime, no permissions are required
  :<|> "auth" :> "touch" 
              :> QueryParam "expire" Seconds
              :> TokenHeader '[]
              :> Post '[JSON] ()
  -- Get client info that is binded to the token
  :<|> "auth" :> "token"
              :> TokenHeader '[]
              :> Get '[JSON] RespUserInfo
  -- Close session, after call of the method the
  -- token in header is not valid.
  :<|> "auth" :> "signout"
              :> TokenHeader '[]
              :> Post '[JSON] ()
  -- Creation of new user, requires 'registerPerm' for token
  :<|> "auth" :> "signup"
              :> ReqBody '[JSON] ReqRegister
              :> TokenHeader '["auth-register"]
              :> Post '[JSON] (OnlyField "user" UserId)
  -- Getting list of all users, requires 'authInfoPerm' for token
  :<|> "auth" :> "users"
              :> PageParam
              :> PageSizeParam
              :> TokenHeader '["auth-info"]
              :> Get '[JSON] RespUsersInfo
  -- Getting info about user, requires 'authInfoPerm' for token
  :<|> "auth" :> "user"
              :> Capture "id" UserId 
              :> TokenHeader '["auth-info"]
              :> Get '[JSON] RespUserInfo
  -- Updating login/email/password, requires 'authUpdatePerm' for token
  :<|> "auth" :> "user"
              :> Capture "id" UserId 
              :> ReqBody '[JSON] PatchUser
              :> TokenHeader '["auth-update"]
              :> Patch '[JSON] ()
  -- Replace user with the user in the body, requires 'authUpdatePerm' for token
  :<|> "auth" :> "user"
              :> Capture "id" UserId 
              :> ReqBody '[JSON] ReqRegister
              :> TokenHeader '["auth-update"]
              :> Put '[JSON] ()
  -- Delete user from DB, requires 'authDeletePerm' and will cause cascade
  -- deletion, that is your usually want
  :<|> "auth" :> "user"
              :> Capture "id" UserId 
              :> TokenHeader '["auth-delete"]
              :> Delete '[JSON] ()
  -- Generate new password for user. There is two phases, first, the method
  -- is called without 'code' parameter. The system sends email with a restore code
  -- to email. After that a call of the method with the code is needed to 
  -- change password. Need configured SMTP server.
  :<|> "auth" :> "restore" 
              :> Capture "id" UserId
              :> QueryParam "code" RestoreCode 
              :> QueryParam "password" Password 
              :> Post '[JSON] ()

  -- Getting info about user group, requires 'authInfoPerm' for token
  :<|> "auth" :> "group"
              :> Capture "id" UserGroupId
              :> TokenHeader '["auth-info"]
              :> Get '[JSON] UserGroup
  -- Inserting new user group, requires 'authUpdatePerm' for token
  :<|> "auth" :> "group"
              :> ReqBody '[JSON] UserGroup
              :> TokenHeader '["auth-update"]
              :> Post '[JSON] UserGroupId
  -- Replace info about given user group, requires 'authUpdatePerm' for token
  :<|> "auth" :> "group"
              :> Capture "id" UserGroupId
              :> ReqBody '[JSON] UserGroup
              :> TokenHeader '["auth-update"]
              :> Put '[JSON] ()
  -- Patch info about given user group, requires 'authUpdatePerm' for token
  :<|> "auth" :> "group"
              :> Capture "id" UserGroupId
              :> ReqBody '[JSON] PatchUserGroup
              :> TokenHeader '["auth-update"]
              :> Patch '[JSON] ()
  -- Delete all info about given user group, requires 'authDeletePerm' for token
  :<|> "auth" :> "group"
              :> Capture "id" UserGroupId
              :> TokenHeader '["auth-delete"]
              :> Delete '[JSON] ()
  -- Get list of user groups, requires 'authInfoPerm' for token 
  :<|> "auth" :> "group"
              :> PageParam
              :> PageSizeParam
              :> TokenHeader '["auth-info"]
              :> Get '[JSON] UserGroups

-- | Proxy type for auth API, used to pass the type-level info into 
-- client/docs generation functions
authAPI :: Proxy AuthAPI 
authAPI = Proxy

-- | Permission that allows everthing by default
adminPerm :: Permission
adminPerm = "admin"

-- | Permission that allows registration of new users
registerPerm :: Permission
registerPerm = "auth-register"

-- | Permission that allows to query info about other users
authInfoPerm :: Permission 
authInfoPerm = "auth-info"

-- | Permission that allows to update fields of an user
authUpdatePerm :: Permission 
authUpdatePerm = "auth-update"

-- | Permission that allows to delete users and cause cascade deletion
authDeletePerm :: Permission
authDeletePerm = "auth-delete"

-- | Select only operations of the Auth API
authOperations :: Traversal' Swagger Operation
authOperations = operationsOf $ toSwagger (Proxy :: Proxy AuthAPI)