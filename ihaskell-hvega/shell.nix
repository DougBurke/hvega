{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc884" }:

let
  inherit (nixpkgs) pkgs;
  inherit (pkgs) haskellPackages;

  package = import ./default.nix { inherit nixpkgs compiler; };

  extra = [ haskellPackages.haskell-language-server
            haskellPackages.cabal-install
          ];
in
  pkgs.stdenv.mkDerivation {
    name = "ihaskell-hvega-shell";
    buildInputs = package.env.nativeBuildInputs ++ extra;
  }
