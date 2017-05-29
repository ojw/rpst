{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc802" }:

let
  inherit (nixpkgs) pkgs;
  ghc = pkgs.haskell.packages.${compiler}.ghcWithHoogle (ps: with ps; [
          lens text present
        ]);
in
(import ./default.nix { inherit nixpkgs compiler; }).env
