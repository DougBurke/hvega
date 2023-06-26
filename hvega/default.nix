{ sources ? import ../nix/sources.nix
, pkgs ? import sources.nixpkgs {}
, compiler ? "ghc928"
}:

let
  # There must be a better way than this! I just want to avoid
  # excessive compilation when using a non-standard compiler.
  #
  isDefaultCompiler = compiler == "ghc8107";  # drop HLS for now

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
      pkgs.haskellPackages.hlint
      pkgs.niv
    ] ++ pkgs.lib.optionals isDefaultCompiler [
      pkgs.haskellPackages.haskell-language-server
    ];
    withHoogle = isDefaultCompiler;
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
