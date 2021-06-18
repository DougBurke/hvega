{ sources ? import ../nix/sources.nix
, pkgs ? import sources.nixpkgs {}
, compiler ? "ghc8104"
}:

let
  # since we are in a sub-directory
  # gitignore = pkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];
  gitignore = pkgs.nix-gitignore.gitignoreSourcePure [ ../.gitignore ];

  myHaskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = hself: hsuper: {
      "ihaskell-hvega" =
        hself.callCabal2nix "ihaskell-hvega" (gitignore ./.) {};

      "hvega" =
        hself.callCabal2nix "hvega" (gitignore ../hvega/.) {};
    };
  };

  # Does it make sense to add hvega/ihaskell to the packages list?
  shell = myHaskellPackages.shellFor {
    packages = p: [
      p."ihaskell-hvega"
      #p.hvega
    ];
    buildInputs = [
      pkgs.haskellPackages.cabal-install
      pkgs.haskellPackages.haskell-language-server
      pkgs.haskellPackages.hlint
      pkgs.haskellPackages.ihaskell
      pkgs.niv
    ];
    withHoogle = true;
  };

  # exe = pkgs.haskell.lib.justStaticExecutables (myHaskellPackages."ihaskell-hvega");

  # docker = pkgs.dockerTools.buildImage {
  #   name = "{{cookiecutter.project_name}}";
  #   config.Cmd = [ "${exe}/bin/{{cookiecutter.project_name}}" ];
  # };
in
{
  inherit shell;
  # inherit exe;
  # inherit docker;
  inherit myHaskellPackages;
  "ihaskell-hvega" = myHaskellPackages."ihaskell-hvega";
}
