#! /usr/bin/env sh

cabal2nix . > rpst.nix
nix-shell --command "cabal configure"
