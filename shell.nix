let
  pkgs = import ./nix {};
  ihaskell = pkgs.ihaskellWithPackages (ps:
    [ ps.pandoc ps.hvega ps.ihaskell-hvega ]);
in pkgs.mkShell
  {
    name = "my-jupyter";
    buildInputs = [ ihaskell ] ;
  }
