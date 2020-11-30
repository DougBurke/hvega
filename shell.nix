let
  sources = import nix/sources.nix;
  pkgs = import sources.nixpkgs {};
  ihaskell = pkgs.ihaskellWithPackages (ps:
    [ ps.pandoc ps.hvega ps.ihaskell-hvega ]);
in pkgs.mkShell
  {
    name = "my-jupyter";
    buildInputs = [ ihaskell ] ;
  }
