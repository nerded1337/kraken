#! /usr/bin/env nix-shell
#! nix-shell -i bash -p cabal2nix cabal-install

make build NUM_JOBS=32

