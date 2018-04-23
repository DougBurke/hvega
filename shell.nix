# Based on the example in https://github.com/nmattia/countries
#
let
  # Create a package set with some overlays
  pkgs = import ./nix;
  ihaskell = pkgs.ihaskellWithPackages
    (ps:
      [
        ps.pandoc
        hvega
        ihaskell-hvega
      ]
    );

  # Set up local modules
  hvega = pkgs.haskellPackages.callPackage ./hvega/default.nix { };
  ihaskell-hvega = pkgs.haskellPackages.callPackage
    ./ihaskell-hvega/default.nix { hvega = hvega; };
  
in pkgs.mkShell
  {
    name = "my-jupyter";
    buildInputs = [ ihaskell ];
  }
