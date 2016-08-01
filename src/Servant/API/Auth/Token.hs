{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
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
  , AuthSigninMethod
  , AuthTouchMethod
  , AuthTokenInfoMethod
  , AuthSignoutMethod
  , AuthSignupMethod
  , AuthUsersMethod
  , AuthGetUserMethod
  , AuthPatchUserMethod
  , AuthPutUserMethod
  , AuthDeleteUserMethod
  , AuthRestoreMethod
  , AuthGetGroupMethod
  , AuthPostGroupMethod
  , AuthPutGroupMethod
  , AuthPatchGroupMethod
  , AuthDeleteGroupMethod
  , AuthGroupsMethod
  , authAPI
  , authDocs
  -- ** Token
  , Token(..)
  , MToken
  , TokenHeader
  , SimpleToken
  -- ** User
  , UserId
  , Login
  , Password
  , Email
  , Permission
  , Seconds
  , RestoreCode
  , ReqRegister(..)
  , RespUserInfo(..)
  , PatchUser(..)
  , RespUsersInfo(..)
  -- ** User groups
  , UserGroupId
  , UserGroup(..)
  , PatchUserGroup(..)
  -- ** Default permissions
  , adminPerm
  , registerPerm
  , authInfoPerm
  , authUpdatePerm
  , authDeletePerm
  -- * Swagger helpers
  , authOperations
  -- * Reexports
  , module Reexport
  ) where 

import Control.Lens
import Data.Aeson.WithField  
import Data.Monoid 
import Data.Proxy
import Data.Swagger (Swagger, Operation)
import Data.Swagger.Internal (SwaggerType(..), _paramSchemaType)
import Data.Swagger.Internal.ParamSchema
import Data.Swagger.Internal.Schema
import Data.Swagger.Operation
import GHC.Generics 
import GHC.TypeLits
import Servant.API
import Servant.Docs 
import Servant.Swagger

import Data.Text (Text)

import Servant.API.Auth.Token.Pagination as Reexport
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

instance ToSample (Token perms) where
  toSamples _ = singleSample s
    where s = Token "123e4567-e89b-12d3-a456-426655440000"

-- | Token that doesn't have attached compile-time permissions
type SimpleToken = Text 
-- | Shortcut for Maybe Token with attached permissions
type MToken (perms :: [Symbol]) = Maybe (Token perms)

-- | User name for login
type Login = Text 
-- | Password for login
type Password = Text
-- | User email
type Email = Text 
-- | Special tag for a permission that a user has
type Permission = Text 
-- | Amount of seconds
type Seconds = Word
-- | Special tag for password restore 
type RestoreCode = Text 

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

instance ToSample ReqRegister where
  toSamples _ = singleSample s
    where 
    s = ReqRegister {
        reqRegLogin = "ncrashed"
      , reqRegPassword = "mydogishappy"
      , reqRegEmail = "ncrashed@gmail.com"
      , reqRegPermissions = ["auth-info", "auth-update"]
      , reqRegGroups = Nothing
      }

-- | Response with user info
data RespUserInfo = RespUserInfo {
  respUserId :: !UserId
, respUserLogin :: !Login 
, respUserEmail :: !Email 
, respUserPermissions :: ![Permission]
, respUserGroups :: ![UserGroupId]
} deriving (Generic, Show)
$(deriveJSON (derivePrefix "respUser") ''RespUserInfo)

instance ToSchema RespUserInfo where 
  declareNamedSchema = genericDeclareNamedSchema $
    schemaOptionsDropPrefix "respUser"

instance ToSample RespUserInfo where
  toSamples _ = singleSample s
    where 
    s = RespUserInfo {
        respUserId = 42
      , respUserLogin = "ncrashed"
      , respUserEmail = "ncrashed@gmail.com"
      , respUserPermissions = ["admin"]
      , respUserGroups = [0, 1]
      }

-- | Response with users info and pagination
data RespUsersInfo = RespUsersInfo {
  respUsersItems :: ![RespUserInfo]
, respUsersPages :: !Word 
} deriving (Generic, Show)
$(deriveJSON (derivePrefix "respUsers") ''RespUsersInfo)

instance ToSchema RespUsersInfo where 
  declareNamedSchema = genericDeclareNamedSchema $
    schemaOptionsDropPrefix "respUsers"

instance ToSample RespUsersInfo where
  toSamples _ = singleSample s
    where 
    s = RespUsersInfo [u] 1
    u = RespUserInfo {
        respUserId = 42
      , respUserLogin = "ncrashed"
      , respUserEmail = "ncrashed@gmail.com"
      , respUserPermissions = ["admin"]
      , respUserGroups = [0, 1]
      }

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

instance ToSample PatchUser where
  toSamples _ = samples [s1, s2, s3]
    where 
    s1 = PatchUser {
        patchUserLogin = Just "nusicrashed"
      , patchUserPassword = Just "mycatishappy"
      , patchUserEmail = Just "ncrashed@mail.ru"
      , patchUserPermissions = Just []
      , patchUserGroups = Nothing
      }
    s2 = PatchUser {
        patchUserLogin = Nothing
      , patchUserPassword = Nothing
      , patchUserEmail = Just "ncrashed@mail.ru"
      , patchUserPermissions = Nothing
      , patchUserGroups = Nothing
      }
    s3 = PatchUser {
        patchUserLogin = Nothing
      , patchUserPassword = Just "mycatishappy"
      , patchUserEmail = Nothing
      , patchUserPermissions = Nothing
      , patchUserGroups = Just [1, 2]
      }

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

instance ToSample UserGroup where
  toSamples _ = singleSample s
    where 
    s = UserGroup {
        userGroupName = "moderators"
      , userGroupUsers = [0, 42, 3]
      , userGroupPermissions = ["auth-register", "auth-update", "auth-delete"]
      , userGroupParent = Nothing
      }

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

instance ToSample PatchUserGroup where
  toSamples _ = samples [s1, s2, s3]
    where 
    s1 = PatchUserGroup {
        patchUserGroupName = Just "developers"
      , patchUserGroupUsers = Just [0, 42, 3]
      , patchUserGroupPermissions = Just ["program", "eat", "sleep"]
      , patchUserGroupParent = Just 2
      , patchUserGroupNoParent = Nothing
      }
    s2 = PatchUserGroup {
        patchUserGroupName = Nothing
      , patchUserGroupUsers = Nothing
      , patchUserGroupPermissions = Just ["program", "sleep"]
      , patchUserGroupParent = Nothing
      , patchUserGroupNoParent = Nothing
      }
    s3 = PatchUserGroup {
        patchUserGroupName = Nothing
      , patchUserGroupUsers = Nothing
      , patchUserGroupPermissions = Nothing
      , patchUserGroupParent = Nothing
      , patchUserGroupNoParent = Just True
      }

instance ToParam (QueryParam "login" Login) where
  toParam _ = DocQueryParam "login" ["ncrashed", "buddy"] "Any valid login for user" Normal
instance ToParam (QueryParam "password" Password) where
  toParam _ = DocQueryParam "password" ["123", "qwerty"] "Any valid password for user" Normal
instance ToParam (QueryParam "expire" Seconds) where
  toParam _ = DocQueryParam "expire" ["600", "30"] "Amount of time in seconds the returned token should be valid for, server can restrain maximum token life" Normal
instance ToParam (QueryParam "code" RestoreCode) where
  toParam _ = DocQueryParam "code" ["fdfygie", "sdf7230"] "Code that was sended to the user by some secure way" Normal

instance ToCapture (Capture "user-id" UserId) where
  toCapture _ = DocCapture "user-id" "unique identifier"
instance ToCapture (Capture "group-id" UserGroupId) where
  toCapture _ = DocCapture "group-id" "identifier of a user group"

-- | Generic authorization API
type AuthAPI =
       AuthSigninMethod
  :<|> AuthTouchMethod
  :<|> AuthTokenInfoMethod
  :<|> AuthSignoutMethod
  :<|> AuthSignupMethod
  :<|> AuthUsersMethod
  :<|> AuthGetUserMethod
  :<|> AuthPatchUserMethod
  :<|> AuthPutUserMethod
  :<|> AuthDeleteUserMethod
  :<|> AuthRestoreMethod
  :<|> AuthGetGroupMethod
  :<|> AuthPostGroupMethod
  :<|> AuthPutGroupMethod
  :<|> AuthPatchGroupMethod
  :<|> AuthDeleteGroupMethod
  :<|> AuthGroupsMethod

-- | How to get a token, expire of 'Nothing' means 
-- some default value (server config)
type AuthSigninMethod = "auth" :> "signin"
  :> QueryParam "login" Login 
  :> QueryParam "password" Password 
  :> QueryParam "expire" Seconds
  :> Get '[JSON] (OnlyField "token" SimpleToken)

-- | Client cat expand the token lifetime, no permissions are required
type AuthTouchMethod = "auth" :> "touch" 
  :> QueryParam "expire" Seconds
  :> TokenHeader '[]
  :> Post '[JSON] ()

-- | Get client info that is binded to the token
type AuthTokenInfoMethod = "auth" :> "token"
  :> TokenHeader '[]
  :> Get '[JSON] RespUserInfo

-- | Close session, after call of the method the
-- token in header is not valid.
type AuthSignoutMethod = "auth" :> "signout"
  :> TokenHeader '[]
  :> Post '[JSON] ()

-- | Creation of new user, requires 'registerPerm' for token
type AuthSignupMethod = "auth" :> "signup"
  :> ReqBody '[JSON] ReqRegister
  :> TokenHeader '["auth-register"]
  :> Post '[JSON] (OnlyField "user" UserId)

-- | Getting list of all users, requires 'authInfoPerm' for token
type AuthUsersMethod = "auth" :> "users"
  :> PageParam
  :> PageSizeParam
  :> TokenHeader '["auth-info"]
  :> Get '[JSON] RespUsersInfo

-- | Getting info about user, requires 'authInfoPerm' for token
type AuthGetUserMethod = "auth" :> "user"
  :> Capture "user-id" UserId 
  :> TokenHeader '["auth-info"]
  :> Get '[JSON] RespUserInfo

-- | Updating login/email/password, requires 'authUpdatePerm' for token
type AuthPatchUserMethod = "auth" :> "user"
  :> Capture "user-id" UserId 
  :> ReqBody '[JSON] PatchUser
  :> TokenHeader '["auth-update"]
  :> Patch '[JSON] ()

-- | Replace user with the user in the body, requires 'authUpdatePerm' for token
type AuthPutUserMethod = "auth" :> "user"
  :> Capture "user-id" UserId 
  :> ReqBody '[JSON] ReqRegister
  :> TokenHeader '["auth-update"]
  :> Put '[JSON] ()

-- | Delete user from DB, requires 'authDeletePerm' and will cause cascade
-- deletion, that is your usually want
type AuthDeleteUserMethod = "auth" :> "user"
  :> Capture "user-id" UserId 
  :> TokenHeader '["auth-delete"]
  :> Delete '[JSON] ()

-- | Generate new password for user. There is two phases, first, the method
-- is called without 'code' parameter. The system sends email with a restore code
-- to user email or sms (its depends on server). After that a call of the method 
-- with the code is needed to change password.
type AuthRestoreMethod = "auth" :> "restore" 
  :> Capture "user-id" UserId
  :> QueryParam "code" RestoreCode 
  :> QueryParam "password" Password 
  :> Post '[JSON] ()

-- | Getting info about user group, requires 'authInfoPerm' for token
type AuthGetGroupMethod = "auth" :> "group"
  :> Capture "group-id" UserGroupId
  :> TokenHeader '["auth-info"]
  :> Get '[JSON] UserGroup

-- | Inserting new user group, requires 'authUpdatePerm' for token
type AuthPostGroupMethod = "auth" :> "group"
  :> ReqBody '[JSON] UserGroup
  :> TokenHeader '["auth-update"]
  :> Post '[JSON] (OnlyId UserGroupId)

-- | Replace info about given user group, requires 'authUpdatePerm' for token
type AuthPutGroupMethod = "auth" :> "group"
  :> Capture "group-id" UserGroupId
  :> ReqBody '[JSON] UserGroup
  :> TokenHeader '["auth-update"]
  :> Put '[JSON] ()

-- | Patch info about given user group, requires 'authUpdatePerm' for token
type AuthPatchGroupMethod = "auth" :> "group"
  :> Capture "group-id" UserGroupId
  :> ReqBody '[JSON] PatchUserGroup
  :> TokenHeader '["auth-update"]
  :> Patch '[JSON] ()

-- | Delete all info about given user group, requires 'authDeletePerm' for token
type AuthDeleteGroupMethod = "auth" :> "group"
  :> Capture "group-id" UserGroupId
  :> TokenHeader '["auth-delete"]
  :> Delete '[JSON] ()

-- | Get list of user groups, requires 'authInfoPerm' for token 
type AuthGroupsMethod = "auth" :> "group"
  :> PageParam
  :> PageSizeParam
  :> TokenHeader '["auth-info"]
  :> Get '[JSON] (PagedList UserGroupId UserGroup)

-- | Proxy type for auth API, used to pass the type-level info into 
-- client/docs generation functions
authAPI :: Proxy AuthAPI 
authAPI = Proxy

-- | Permission that allows everything by default
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

-- | "Servant.Docs" documentation of the Auth API
authDocs :: API
authDocs = docsWith defaultDocOptions [intro] extra (Proxy :: Proxy AuthAPI)
  where 
  intro = DocIntro "Authorisation API by token"
    [ "The API provides stateless way to implement authorisation for RESTful APIs. A user of the API get a token once and can query other methods of server only providing the token until it expires."
    , "Also the API provides a way to pack users in hierarchy of groups with attached permissions."
    ]
  extra = 
       mkExtra (Proxy :: Proxy AuthSigninMethod) "How to get a token, missing expire means some default value (server config)"
    <> mkExtra (Proxy :: Proxy AuthTouchMethod) "Client cat expand the token lifetime, no permissions are required"
    <> mkExtra (Proxy :: Proxy AuthTokenInfoMethod) "Get client info that is binded to the token"
    <> mkExtra (Proxy :: Proxy AuthSignoutMethod) "Close session, after call of the method the token in header is not valid."
    <> mkExtra (Proxy :: Proxy AuthSignupMethod) "Creation of new user, requires 'registerPerm' for token"
    <> mkExtra (Proxy :: Proxy AuthUsersMethod) "Getting list of all users, requires 'authInfoPerm' for token"
    <> mkExtra (Proxy :: Proxy AuthGetUserMethod) "Getting info about user, requires 'authInfoPerm' for token"
    <> mkExtra (Proxy :: Proxy AuthPatchUserMethod) "Updating login/email/password, requires 'authUpdatePerm' for token"
    <> mkExtra (Proxy :: Proxy AuthPutUserMethod) "Replace user with the user in the body, requires 'authUpdatePerm' for token"
    <> mkExtra (Proxy :: Proxy AuthDeleteUserMethod) "Delete user from DB, requires 'authDeletePerm' and will cause cascade deletion, that is your usually want"
    <> mkExtra (Proxy :: Proxy AuthRestoreMethod) "Generate new password for user. There is two phases, first, the method is called without 'code' parameter. The system sends email with a restore code to user email or sms (its depends on server). After that a call of the method with the code is needed to change password."
    <> mkExtra (Proxy :: Proxy AuthGetGroupMethod) "Getting info about user group, requires 'authInfoPerm' for token"
    <> mkExtra (Proxy :: Proxy AuthPostGroupMethod) "Inserting new user group, requires 'authUpdatePerm' for token"
    <> mkExtra (Proxy :: Proxy AuthPutGroupMethod) "Replace info about given user group, requires 'authUpdatePerm' for token"
    <> mkExtra (Proxy :: Proxy AuthPatchGroupMethod) "Patch info about given user group, requires 'authUpdatePerm' for token"
    <> mkExtra (Proxy :: Proxy AuthDeleteGroupMethod) "Delete all info about given user group, requires 'authDeletePerm' for token"
    <> mkExtra (Proxy :: Proxy AuthGroupsMethod) "Get list of user groups, requires 'authInfoPerm' for token "

  mkExtra p s = extraInfo p $  
    defAction & notes <>~ [ DocNote "Description" [s] ]

instance ToSample Word where 
  toSamples _ = samples [0, 4, 8, 15, 16, 23, 42]

instance ToSample Text where 
  toSamples _ = samples ["", "some text", "magic"]