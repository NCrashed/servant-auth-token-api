## Authorisation API by token

The API provides stateless way to implement authorisation for RESTful APIs. A user of the API get a token once and can query other methods of server only providing the token until it expires.

Also the API provides a way to pack users in hierarchy of groups with attached permissions.

## GET /auth/group

#### Description

Get list of user groups, requires 'authInfoPerm' for token 

#### Authentication



Clients must supply the following data



- This endpoint is sensitive to the value of the **Authorization** HTTP header.

#### GET Parameters:

- page
     - **Values**: *0, 42*
     - **Description**: Index of page

- size
     - **Values**: *42, 10*
     - **Description**: Number of elements on page


#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
{"items":[{"parent":null,"users":[0,42,3],"name":"moderators","id":0,"permissions":["auth-register","auth-update","auth-delete"]},{"parent":null,"users":[0,42,3],"name":"moderators","id":4,"permissions":["auth-register","auth-update","auth-delete"]},{"parent":null,"users":[0,42,3],"name":"moderators","id":8,"permissions":["auth-register","auth-update","auth-delete"]},{"parent":null,"users":[0,42,3],"name":"moderators","id":15,"permissions":["auth-register","auth-update","auth-delete"]},{"parent":null,"users":[0,42,3],"name":"moderators","id":16,"permissions":["auth-register","auth-update","auth-delete"]},{"parent":null,"users":[0,42,3],"name":"moderators","id":23,"permissions":["auth-register","auth-update","auth-delete"]},{"parent":null,"users":[0,42,3],"name":"moderators","id":42,"permissions":["auth-register","auth-update","auth-delete"]}],"pages":1}
```

## POST /auth/group

#### Description

Inserting new user group, requires 'authUpdatePerm' for token

#### Authentication



Clients must supply the following data



- This endpoint is sensitive to the value of the **Authorization** HTTP header.

#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

```javascript
{"name":"moderators","users":[0,42,3],"permissions":["auth-register","auth-update","auth-delete"],"parent":null}
```

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- 

```javascript
{"id":0}
```

- 

```javascript
{"id":4}
```

- 

```javascript
{"id":8}
```

- 

```javascript
{"id":15}
```

- 

```javascript
{"id":16}
```

## DELETE /auth/group/:group-id

#### Description

Delete all info about given user group, requires 'authDeletePerm' for token

#### Authentication



Clients must supply the following data


#### Captures:

- *group-id*: identifier of a user group


- This endpoint is sensitive to the value of the **Authorization** HTTP header.

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
[]
```

## GET /auth/group/:group-id

#### Description

Getting info about user group, requires 'authInfoPerm' for token

#### Authentication



Clients must supply the following data


#### Captures:

- *group-id*: identifier of a user group


- This endpoint is sensitive to the value of the **Authorization** HTTP header.

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
{"name":"moderators","users":[0,42,3],"permissions":["auth-register","auth-update","auth-delete"],"parent":null}
```

## PATCH /auth/group/:group-id

#### Description

Patch info about given user group, requires 'authUpdatePerm' for token

#### Authentication



Clients must supply the following data


#### Captures:

- *group-id*: identifier of a user group


- This endpoint is sensitive to the value of the **Authorization** HTTP header.

#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

```javascript
{"name":"developers","users":[0,42,3],"permissions":["program","eat","sleep"],"parent":2,"noParent":null}
```

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
[]
```

## PUT /auth/group/:group-id

#### Description

Replace info about given user group, requires 'authUpdatePerm' for token

#### Authentication



Clients must supply the following data


#### Captures:

- *group-id*: identifier of a user group


- This endpoint is sensitive to the value of the **Authorization** HTTP header.

#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

```javascript
{"name":"moderators","users":[0,42,3],"permissions":["auth-register","auth-update","auth-delete"],"parent":null}
```

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
[]
```

## POST /auth/restore/:user-id

#### Description

Generate new password for user. There is two phases, first, the method is called without 'code' parameter. The system sends email with a restore code to user email or sms (its depends on server). After that a call of the method with the code is needed to change password.

#### Authentication



Clients must supply the following data


#### Captures:

- *user-id*: unique identifier

#### GET Parameters:

- code
     - **Values**: *fdfygie, sdf7230*
     - **Description**: Code that was sended to the user by some secure way

- password
     - **Values**: *123, qwerty*
     - **Description**: Any valid password for user


#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
[]
```

## GET /auth/signin

#### Description

How to get a token, missing expire means some default value (server config)

#### Authentication



Clients must supply the following data


#### GET Parameters:

- login
     - **Values**: *ncrashed, buddy*
     - **Description**: Any valid login for user

- password
     - **Values**: *123, qwerty*
     - **Description**: Any valid password for user

- expire
     - **Values**: *600, 30*
     - **Description**: Amount of time in seconds the returned token should be valid for, server can restrain maximum token life


#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- 

```javascript
{"token":""}
```

- 

```javascript
{"token":"some text"}
```

- 

```javascript
{"token":"magic"}
```

## POST /auth/signout

#### Description

Close session, after call of the method the token in header is not valid.

#### Authentication



Clients must supply the following data



- This endpoint is sensitive to the value of the **Authorization** HTTP header.

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
[]
```

## POST /auth/signup

#### Description

Creation of new user, requires 'registerPerm' for token

#### Authentication



Clients must supply the following data



- This endpoint is sensitive to the value of the **Authorization** HTTP header.

#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

```javascript
{"login":"ncrashed","password":"mydogishappy","email":"ncrashed@gmail.com","permissions":["auth-info","auth-update"],"groups":null}
```

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- 

```javascript
{"user":0}
```

- 

```javascript
{"user":4}
```

- 

```javascript
{"user":8}
```

- 

```javascript
{"user":15}
```

- 

```javascript
{"user":16}
```

## GET /auth/token

#### Description

Get client info that is binded to the token

#### Authentication



Clients must supply the following data



- This endpoint is sensitive to the value of the **Authorization** HTTP header.

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
{"id":42,"login":"ncrashed","email":"ncrashed@gmail.com","permissions":["admin"],"groups":[0,1]}
```

## POST /auth/touch

#### Description

Client cat expand the token lifetime, no permissions are required

#### Authentication



Clients must supply the following data



- This endpoint is sensitive to the value of the **Authorization** HTTP header.

#### GET Parameters:

- expire
     - **Values**: *600, 30*
     - **Description**: Amount of time in seconds the returned token should be valid for, server can restrain maximum token life


#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
[]
```

## DELETE /auth/user/:user-id

#### Description

Delete user from DB, requires 'authDeletePerm' and will cause cascade deletion, that is your usually want

#### Authentication



Clients must supply the following data


#### Captures:

- *user-id*: unique identifier


- This endpoint is sensitive to the value of the **Authorization** HTTP header.

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
[]
```

## GET /auth/user/:user-id

#### Description

Getting info about user, requires 'authInfoPerm' for token

#### Authentication



Clients must supply the following data


#### Captures:

- *user-id*: unique identifier


- This endpoint is sensitive to the value of the **Authorization** HTTP header.

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
{"id":42,"login":"ncrashed","email":"ncrashed@gmail.com","permissions":["admin"],"groups":[0,1]}
```

## PATCH /auth/user/:user-id

#### Description

Updating login/email/password, requires 'authUpdatePerm' for token

#### Authentication



Clients must supply the following data


#### Captures:

- *user-id*: unique identifier


- This endpoint is sensitive to the value of the **Authorization** HTTP header.

#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

```javascript
{"login":"nusicrashed","password":"mycatishappy","email":"ncrashed@mail.ru","permissions":[],"groups":null}
```

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
[]
```

## PUT /auth/user/:user-id

#### Description

Replace user with the user in the body, requires 'authUpdatePerm' for token

#### Authentication



Clients must supply the following data


#### Captures:

- *user-id*: unique identifier


- This endpoint is sensitive to the value of the **Authorization** HTTP header.

#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

```javascript
{"login":"ncrashed","password":"mydogishappy","email":"ncrashed@gmail.com","permissions":["auth-info","auth-update"],"groups":null}
```

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
[]
```

## GET /auth/users

#### Description

Getting list of all users, requires 'authInfoPerm' for token

#### Authentication



Clients must supply the following data



- This endpoint is sensitive to the value of the **Authorization** HTTP header.

#### GET Parameters:

- page
     - **Values**: *0, 42*
     - **Description**: Index of page

- size
     - **Values**: *42, 10*
     - **Description**: Number of elements on page


#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
{"items":[{"id":42,"login":"ncrashed","email":"ncrashed@gmail.com","permissions":["admin"],"groups":[0,1]}],"pages":1}
```

