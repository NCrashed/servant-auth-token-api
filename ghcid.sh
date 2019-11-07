PACKAGE=${1:-servant-auth-token-api}
ghcid -c "cabal new-repl --ghc-options='-Werror -Wall' $PACKAGE "
