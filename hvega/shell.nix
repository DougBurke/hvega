{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc8104" }:
(import ./default.nix { inherit nixpkgs compiler; }).shell
