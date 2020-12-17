{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc8102" }:
(import ./default.nix { inherit nixpkgs compiler; }).shell
