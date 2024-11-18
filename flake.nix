{
  # inspired by: https://serokell.io/blog/practical-nix-flakes#packaging-existing-applications
  description = "explore hvega and IHaskell";
  
  inputs = {
    nixpkgs.url = "nixpkgs";
    # IHaskell.url = "github:IHaskell/IHaskell";
  };
  
  # outputs = { self, nixpkgs, IHaskell }:
  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" "x86_64-darwin" ];
      # supportedSystems = [ "x86_64-linux" ];
      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);
      nixpkgsFor = forAllSystems (system: import nixpkgs {
        inherit system;
        overlays = [ self.overlay ];
      });
    in
    {
      overlay = (final: prev: {
        hvega = final.haskellPackages.callCabal2nix "hvega" ./hvega {};
        ihaskell-hvega = final.haskellPackages.callCabal2nix "ihaskell-hvega" ./ihaskell-hvega {};
      });
      packages = forAllSystems (system: {
         hvega = nixpkgsFor.${system}.hvega;
         ihaskell-hvega = nixpkgsFor.${system}.ihaskell-hvega;
      });
      # what do we want for the defaultPackage here?
      defaultPackage = forAllSystems (system: self.packages.${system}.hvega);
      checks = self.packages;
      devShell = forAllSystems (system: let haskellPackages = nixpkgsFor.${system}.haskellPackages;
        in haskellPackages.shellFor {
          packages = p: [self.packages.${system}.hvega
	                 self.packages.${system}.ihaskell-hvega];
          # withHoogle = true;
	  withHoogle = false;  # faster ...
          buildInputs = with haskellPackages; [
            haskell-language-server
            hlint
            cabal-install
	    ihaskell
	    nixpkgsFor.${system}.jupyter
          ];
        # Change the prompt to show that you are in a devShell
        shellHook = ''
  echo -e "*** \e[1;32mWelcome to hvega development\e[0m ***"
  ghc --version
  cabal --version
  hlint --version
  echo -e "IHaskell: `ihaskell --version`"
  echo -e ""
  export PS1='hvegadev:\A \e[1;34m\w\e[0m '
	'';
        });
  };
}
