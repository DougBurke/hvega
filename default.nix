let
  sources = import nix/sources.nix;
  pkgs = import sources.nixpkgs {};

  # Use the local hvega/ihaskell-hvega rather than the nixpkgs ones.
  # I should probably be using an override.
  #
  hvegaSource = ./.;
  hvegaDir = hvegaSource + "/hvega";
  ihaskellvegaDir = hvegaSource + "/ihaskell-hvega";

  gitignore = pkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];
  ps = pkgs.haskellPackages;
  hvega = ps.callCabal2nix "hvega" (gitignore hvegaDir) {};
  ihaskell-hvega = ps.callCabal2nix "ihaskell-hvega" (gitignore ihaskellvegaDir) {};

in import "${sources.IHaskell}/release.nix" {
  compiler = "ghc8104";
  nixpkgs = pkgs;
  packages = p: [ hvega ihaskell-hvega ];
  # packages = p: [ hvega ihaskell-hvega p.aeson p.aeson-pretty p.Frames p.foldl p.formatting p.microlens ];
  # packages = self: with self; [ hvega ihaskell-hvega ];
  # systemPackages = self: with self; [ graphviz ];
}
