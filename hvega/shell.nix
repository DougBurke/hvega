{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc882" }:
(import ./default.nix { inherit nixpkgs compiler; }).env
