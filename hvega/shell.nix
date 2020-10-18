{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc884" }:
(import ./default.nix { inherit nixpkgs compiler; }).env
