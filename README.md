# servant-auth-token-api

[![Build Status](https://travis-ci.org/NCrashed/servant-auth-token-api.svg?branch=master)](https://travis-ci.org/NCrashed/servant-auth-token-api)

The package provides abstract RESTful API for token based authorisation using [servant](http://haskell-servant.readthedocs.io/en/stable/). It contains only generic types and [servant-swagger](https://hackage.haskell.org/package/servant-swagger) instances. You may be interested in server/client side implementations:

- [servant-auth-token](https://github.com/NCrashed/servant-auth-token) - server side implementation using SQL subdomain of [persistent](http://hackage.haskell.org/package/persistent).

- servant-auth-token-client (not yet published) - client side implementation using GHCJS and [reflex-dom](https://hackage.haskell.org/package/reflex-dom)

Also you can explore [swagger documentation](https://ncrashed.github.io/servant-auth-token-api/swagger-ui) for the API and view [markdown version of docs](https://github.com/NCrashed/servant-auth-token-api/blob/gh-pages/docs.markdown).