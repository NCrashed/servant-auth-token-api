{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
#endif
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
  , AuthSigninGetCodeMethod
  , AuthSigninPostCodeMethod
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
  -- ** Permission symbol
  , PermSymbol(..)
  , UnliftPermSymbol(..)
  , PermsList(..)
  , PlainPerms
  -- ** Token
  , Token(..)
  , Token'
  , MToken
  , MToken'
  , TokenHeader
  , TokenHeader'
  , SimpleToken
  , downgradeToken'
  , downgradeToken
  -- ** User
  , UserId
  , Login
  , Password
  , Email
  , Permission
  , Seconds
  , RestoreCode
  , SingleUseCode
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
  ) where 

import Control.Lens
import Data.Aeson.Unit
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
import Text.RawString.QQ

import Data.Text (Text)
import qualified Data.Text as T 

import Servant.API.Auth.Token.Pagination
import Servant.API.Auth.Token.Internal.DeriveJson 
import Servant.API.Auth.Token.Internal.Schema

instance ToSample Unit where 
  toSamples _ = singleSample Unit

-- | Type level permission type that allows to 
-- construct complex permission labels
data {- kind -} PermSymbol = 
    PermLabel Symbol 
  | PermConcat PermSymbol PermSymbol

-- | Convertation of permission symbol into runtim string
class UnliftPermSymbol (s :: PermSymbol) where 
  unliftPermSymbol :: Proxy s -> String 

instance KnownSymbol s => UnliftPermSymbol ('PermLabel s) where 
  unliftPermSymbol _ = symbolVal (Proxy :: Proxy s)

instance (UnliftPermSymbol p1, UnliftPermSymbol p2) => UnliftPermSymbol ('PermConcat p1 p2) where 
  unliftPermSymbol _ = unliftPermSymbol (Proxy :: Proxy p1)
    ++ unliftPermSymbol (Proxy :: Proxy p2)

-- | Helper type family to wrap all symbols into 'PermLabel'
type family PlainPerms (p :: [Symbol]) :: [PermSymbol] where 
  PlainPerms '[] = '[]
  PlainPerms (s ': ss) = 'PermLabel s ': PlainPerms ss

-- | Token is simple string marked by permissions that are expected
-- from the token to pass guarding functions.
newtype Token (perms :: [PermSymbol]) = Token { unToken :: Text }
  deriving (Eq, Show)

-- | Token is simple string marked by permissions that are expected
-- from the token to pass guarding functions.
--
-- Simplified version that takes plain symbols as permissions.
type Token' (perms :: [Symbol]) = Token (PlainPerms perms)

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
type MToken (perms :: [PermSymbol]) = Maybe (Token perms)

-- | Shortcut for Maybe Token with attached permissions
--
-- Simplified version that takes plain symbols as permissions.
type MToken' (perms :: [Symbol]) = MToken (PlainPerms perms)

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
-- | Single use code used for authorisation via 'AuthSigninGetCodeMethod' and
-- 'AuthSigninPostCodeMethod' endpoints
type SingleUseCode = Text 

-- | Token header that we require for authorization marked 
-- by permissions that are expected from the token to pass guarding functions.
type TokenHeader (perms :: [PermSymbol]) = 
  Header "Authorization" (Token perms)

-- | Token header that we require for authorization marked 
-- by permissions that are expected from the token to pass guarding functions.
--
-- Simplified version that takes plain symbols as permissions.
type TokenHeader' (perms :: [Symbol]) = TokenHeader (PlainPerms perms)

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
  :<|> AuthSigninGetCodeMethod
  :<|> AuthSigninPostCodeMethod
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
-- some default value (server config).
--
-- Logic of authorisation via this method is:
--
-- * Client sends GET request to the endpoint with
-- user specified login and password and optional expire
--
-- * Server responds with token or error
--
-- * Client uses the token with other requests as authorisation
-- header
--
-- * Client can extend lifetime of token by periodically pinging
-- of 'AuthTouchMethod' endpoint
--
-- * Client can invalidate token instantly by 'AuthSignoutMethod'
--
-- * Client can get info about user with 'AuthTokenInfoMethod' endpoint.
type AuthSigninMethod = "auth" :> "signin"
  :> QueryParam "login" Login 
  :> QueryParam "password" Password 
  :> QueryParam "expire" Seconds
  :> Get '[JSON] (OnlyField "token" SimpleToken)

-- | Authorisation via code of single usage.
--
-- Logic of authorisation via this method is:
-- 
-- * Client sends GET request to 'AuthSigninGetCodeMethod' endpoint
--
-- * Server generates single use token and sends it via
--   SMS or email (server specific implementation)
--
-- * Client sends POST request to 'AuthSigninPostCodeMethod' endpoint
--
-- * Server responds with auth token.
--
-- * Client uses the token with other requests as authorisation
-- header
--
-- * Client can extend lifetime of token by periodically pinging
-- of 'AuthTouchMethod' endpoint
--
-- * Client can invalidate token instantly by 'AuthSignoutMethod'
--
-- * Client can get info about user with 'AuthTokenInfoMethod' endpoint.
type AuthSigninGetCodeMethod = "auth" :> "signin"
  :> QueryParam "login" Login 
  :> Get '[JSON] Unit

-- | Authorisation via code of single usage.
--
-- Logic of authorisation via this method is:
-- 
-- * Client sends GET request to 'AuthSigninGetCodeMethod' endpoint
--
-- * Server generates single use token and sends it via
--   SMS or email (server specific implementation)
--
-- * Client sends POST request to 'AuthSigninPostCodeMethod' endpoint
--
-- * Server responds with auth token.
--
-- * Client uses the token with other requests as authorisation
-- header
--
-- * Client can extend lifetime of token by periodically pinging
-- of 'AuthTouchMethod' endpoint
--
-- * Client can invalidate token instantly by 'AuthSignoutMethod'
--
-- * Client can get info about user with 'AuthTokenInfoMethod' endpoint.
type AuthSigninPostCodeMethod = "auth" :> "signin"
  :> QueryParam "login" Login 
  :> QueryParam "code" SingleUseCode
  :> QueryParam "expire" Seconds
  :> Post '[JSON] (OnlyField "token" SimpleToken)

-- | Client cat expand the token lifetime, no permissions are required
type AuthTouchMethod = "auth" :> "touch" 
  :> QueryParam "expire" Seconds
  :> TokenHeader '[]
  :> Post '[JSON] Unit

-- | Get client info that is binded to the token
type AuthTokenInfoMethod = "auth" :> "token"
  :> TokenHeader '[]
  :> Get '[JSON] RespUserInfo

-- | Close session, after call of the method the
-- token in header is not valid.
type AuthSignoutMethod = "auth" :> "signout"
  :> TokenHeader '[]
  :> Post '[JSON] Unit

-- | Creation of new user, requires 'registerPerm' for token
type AuthSignupMethod = "auth" :> "signup"
  :> ReqBody '[JSON] ReqRegister
  :> TokenHeader' '["auth-register"]
  :> Post '[JSON] (OnlyField "user" UserId)

-- | Getting list of all users, requires 'authInfoPerm' for token
type AuthUsersMethod = "auth" :> "users"
  :> PageParam
  :> PageSizeParam
  :> TokenHeader' '["auth-info"]
  :> Get '[JSON] RespUsersInfo

-- | Getting info about user, requires 'authInfoPerm' for token
type AuthGetUserMethod = "auth" :> "user"
  :> Capture "user-id" UserId 
  :> TokenHeader' '["auth-info"]
  :> Get '[JSON] RespUserInfo

-- | Updating login/email/password, requires 'authUpdatePerm' for token
type AuthPatchUserMethod = "auth" :> "user"
  :> Capture "user-id" UserId 
  :> ReqBody '[JSON] PatchUser
  :> TokenHeader' '["auth-update"]
  :> Patch '[JSON] Unit

-- | Replace user with the user in the body, requires 'authUpdatePerm' for token
type AuthPutUserMethod = "auth" :> "user"
  :> Capture "user-id" UserId 
  :> ReqBody '[JSON] ReqRegister
  :> TokenHeader' '["auth-update"]
  :> Put '[JSON] Unit

-- | Delete user from DB, requires 'authDeletePerm' and will cause cascade
-- deletion, that is your usually want
type AuthDeleteUserMethod = "auth" :> "user"
  :> Capture "user-id" UserId 
  :> TokenHeader' '["auth-delete"]
  :> Delete '[JSON] Unit

-- | Generate new password for user. There is two phases, first, the method
-- is called without 'code' parameter. The system sends email with a restore code
-- to user email or sms (its depends on server). After that a call of the method 
-- with the code is needed to change password.
type AuthRestoreMethod = "auth" :> "restore" 
  :> Capture "user-id" UserId
  :> QueryParam "code" RestoreCode 
  :> QueryParam "password" Password 
  :> Post '[JSON] Unit

-- | Getting info about user group, requires 'authInfoPerm' for token
type AuthGetGroupMethod = "auth" :> "group"
  :> Capture "group-id" UserGroupId
  :> TokenHeader' '["auth-info"]
  :> Get '[JSON] UserGroup

-- | Inserting new user group, requires 'authUpdatePerm' for token
type AuthPostGroupMethod = "auth" :> "group"
  :> ReqBody '[JSON] UserGroup
  :> TokenHeader' '["auth-update"]
  :> Post '[JSON] (OnlyId UserGroupId)

-- | Replace info about given user group, requires 'authUpdatePerm' for token
type AuthPutGroupMethod = "auth" :> "group"
  :> Capture "group-id" UserGroupId
  :> ReqBody '[JSON] UserGroup
  :> TokenHeader' '["auth-update"]
  :> Put '[JSON] Unit

-- | Patch info about given user group, requires 'authUpdatePerm' for token
type AuthPatchGroupMethod = "auth" :> "group"
  :> Capture "group-id" UserGroupId
  :> ReqBody '[JSON] PatchUserGroup
  :> TokenHeader' '["auth-update"]
  :> Patch '[JSON] Unit

-- | Delete all info about given user group, requires 'authDeletePerm' for token
type AuthDeleteGroupMethod = "auth" :> "group"
  :> Capture "group-id" UserGroupId
  :> TokenHeader' '["auth-delete"]
  :> Delete '[JSON] Unit

-- | Get list of user groups, requires 'authInfoPerm' for token 
type AuthGroupsMethod = "auth" :> "group"
  :> PageParam
  :> PageSizeParam
  :> TokenHeader' '["auth-info"]
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
       mkExtra' (Proxy :: Proxy AuthSigninMethod) ["How to get a token, missing expire means some default value (server config).", simpleAuthDescr]
    <> mkExtra' (Proxy :: Proxy AuthSigninGetCodeMethod) ["Authorisation via codes of single usage, that are sended to user via different channel of communication.", singleUseAuthDescr]
    <> mkExtra' (Proxy :: Proxy AuthSigninPostCodeMethod) ["Authorisation via codes of single usage, that are sended to user via different channel of communication.", singleUseAuthDescr]
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
  mkExtra' p ss = extraInfo p $  
    defAction & notes <>~ [ DocNote "Description" ss ]

  simpleAuthDescr = [r|
    Logic of authorisation via login and password method is:

    * Client sends GET request to the endpoint with
    user specified login and password and optional expire
   
    * Server responds with token or error
   
    * Client uses the token with other requests as authorisation
    header
   
    * Client can extend lifetime of token by periodically pinging
    of 'AuthTouchMethod' endpoint
   
    * Client can invalidate token instantly by 'AuthSignoutMethod'
   
    * Client can get info about user with 'AuthTokenInfoMethod' endpoint.
    |]

  singleUseAuthDescr = [r|
    Logic of authorisation via single use code method is:
    
    * Client sends GET request to 'AuthSigninGetCodeMethod' endpoint
   
    * Server generates single use token and sends it via
      SMS or email (server specific implementation)
   
    * Client sends POST request to 'AuthSigninPostCodeMethod' endpoint
   
    * Server responds with auth token.
   
    * Client uses the token with other requests as authorisation
    header
   
    * Client can extend lifetime of token by periodically pinging
    of 'AuthTouchMethod' endpoint
   
    * Client can invalidate token instantly by 'AuthSignoutMethod'
   
    * Client can get info about user with 'AuthTokenInfoMethod' endpoint.
    |]

instance ToSample Word where 
  toSamples _ = samples [0, 4, 8, 15, 16, 23, 42]

instance ToSample Text where 
  toSamples _ = samples ["", "some text", "magic"]

#if MIN_VERSION_servant_docs(0,8,0)
instance ToSample () where 
  toSamples _ = singleSample ()
#endif

-- | Unlifting compile-time permissions into list of run-time permissions
class PermsList (a :: [PermSymbol]) where 
  unliftPerms :: forall proxy . proxy a -> [Permission]

instance PermsList '[] where 
  unliftPerms _ = []

instance (UnliftPermSymbol x, PermsList xs) => PermsList (x ': xs) where 
  unliftPerms _ = T.pack (unliftPermSymbol (Proxy :: Proxy x))
    : unliftPerms (Proxy :: Proxy xs)

-- | Check whether a 'b' is contained in permission list of 'a'
type family ContainPerm (a :: [PermSymbol]) (b :: PermSymbol) where 
  ContainPerm '[] b = 'False
  ContainPerm (a ': as) a = 'True
  ContainPerm (a ': as) b = ContainPerm as b

-- | Check that first set of permissions is subset of second
type family ConatinAllPerm (a :: [PermSymbol]) (b :: [PermSymbol]) where 
  ConatinAllPerm '[] bs = '[]
  ConatinAllPerm (a ': as) bs = (ContainPerm bs a) ': (ConatinAllPerm as bs)

-- | Foldl type level list of bools, identicall to 'and'
type family TAll (a :: [Bool]) :: Bool where 
  TAll '[] = 'True
  TAll ('True ': as) = TAll as 
  TAll ('False ': as) = 'False 

-- | Check that first set of permissions is subset of second, throw error if not
type PermsSubset (a :: [PermSymbol]) (b :: [PermSymbol]) = TAll (ConatinAllPerm a b)

-- | Cast token to permissions that are lower than original one
--
-- The cast is safe, the permissions are cheked on compile time.
downgradeToken' :: 'True ~ PermsSubset ts' ts => Token ts -> Token ts' 
downgradeToken' = Token . unToken 

-- | Cast token to permissions that are lower than original one.
--
-- The cast is safe, the permissions are cheked on compile time.
downgradeToken :: 'True ~ PermsSubset ts' ts => MToken ts -> MToken ts'
downgradeToken = fmap downgradeToken'