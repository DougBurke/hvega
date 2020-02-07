{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc882" }:
nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./hvega.nix { }
