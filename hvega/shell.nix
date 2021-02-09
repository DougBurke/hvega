{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc8103" }:
(import ./default.nix { inherit nixpkgs compiler; }).shell
