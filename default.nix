let
  sources = import nix/sources.nix;
in import "${sources.IHaskell}/release.nix" {
  compiler = "ghc8104";
  nixpkgs  = import sources.nixpkgs {};
  packages = self: with self; [ hvega ihaskell-hvega ];
  # systemPackages = self: with self; [ graphviz ];
}
