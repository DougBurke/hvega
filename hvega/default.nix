{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc884" }:

let
  hp = nixpkgs.pkgs.haskell.packages.${compiler};

in
  hp.callCabal2nix "hvega" (./.) {}
