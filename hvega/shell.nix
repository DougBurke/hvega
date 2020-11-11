{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc884" }:

let
  inherit (nixpkgs) pkgs;
  inherit (pkgs) haskellPackages;

  hvega = import ./default.nix { inherit nixpkgs compiler; };

  extra = [ haskellPackages.haskell-language-server
            haskellPackages.cabal-install
          ];
in
  pkgs.stdenv.mkDerivation {
    name = "hvega-shell";
    buildInputs = hvega.env.nativeBuildInputs ++ extra;
  }
