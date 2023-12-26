{
  # inspired by: https://serokell.io/blog/practical-nix-flakes#packaging-existing-applications
  description = "Allow vega visualizations to auto-display in IHaskell";
  inputs.nixpkgs.url = "nixpkgs";
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
        ihaskell-hvega = final.haskellPackages.callCabal2nix "ihaskell-hvega" ./. {};
        hvega = final.haskellPackages.callCabal2nix "../hvega" ./. {};
      });
      packages = forAllSystems (system: {
         # do we need to set/override both?
         hvega = nixpkgsFor.${system}.hvega;
         ihaskell-hvega = nixpkgsFor.${system}.ihaskell-hvega;
      });
      defaultPackage = forAllSystems (system: self.packages.${system}.ihaskell-hvega);
      checks = self.packages;
      devShell = forAllSystems (system: let haskellPackages = nixpkgsFor.${system}.haskellPackages;
        in haskellPackages.shellFor {
          packages = p: [self.packages.${system}.ihaskell-hvega];
          withHoogle = true;
          buildInputs = with haskellPackages; [
            haskell-language-server
            hlint
            cabal-install
          ];
        # Change the prompt to show that you are in a devShell
        shellHook = ''
  echo -e "*** \e[1;32mWelcome to ihaskell-hvega\e[0m ***"
  ghc --version
  cabal --version
  hlint --version
  echo -e ""
  export PS1='ihaskell-hvega:\A \e[1;34m\w\e[0m '
	'';
        });
  };
}
