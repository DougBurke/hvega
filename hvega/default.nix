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
      "hvega" =
        hself.callCabal2nix "hvega" (gitignore ./.) {};
    };
  };

  shell = myHaskellPackages.shellFor {
    packages = p: [
      p."hvega"
    ];
    buildInputs = [
      pkgs.haskellPackages.cabal-install
      pkgs.haskellPackages.haskell-language-server
      pkgs.haskellPackages.hlint
      pkgs.niv
    ];
    withHoogle = true;
  };

  # exe = pkgs.haskell.lib.justStaticExecutables (myHaskellPackages."hvega");

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
  "hvega" = myHaskellPackages."hvega";
}
