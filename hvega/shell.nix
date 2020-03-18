{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc883" }:
(import ./default.nix { inherit nixpkgs compiler; }).env
