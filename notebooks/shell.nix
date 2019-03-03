let

  jupyterLibPath = builtins.fetchGit {
    url = https://github.com/tweag/jupyterWith;
    rev = "";
  };

  nixpkgsPath = jupyterLibPath + "/nix";
  
  pkgs = import nixpkgsPath {};
  jupyter = import jupyterLibPath { pkgs=pkgs; };

  iPython = jupyter.kernels.iPythonWith {
    name = "python";
    packages = p: with p; [ numpy ];
  };

  # I haven't released the updated ihaskell-vega yet, so need to grab
  # it from GitHub.
  #
  hvegaSource = builtins.fetchGit {
    url = https://github.com/DougBurke/hvega;
    rev = "";
  };

  hvegaDir = hvegaSource + "/hvega";
  ihaskellvegaDir = hvegaSource + "/ihaskell-hvega";
  
  haskellPackages = pkgs.haskellPackages.override (old: {
    overrides = pkgs.lib.composeExtensions old.overrides
      (self: ps: {
        hvega = ps.callCabal2nix "hvega" hvegaDir {};
        ihaskell-hvega = ps.callCabal2nix "ihaskell-hvega" ihaskellvegaDir {};
      });
    });
    
  iHaskell = jupyter.kernels.iHaskellWith {
    name = "haskell";
    haskellPackages = haskellPackages;
    packages = p: with p; [ hvega ihaskell-hvega formatting ];
  };

  jupyterEnvironment =
    jupyter.jupyterlabWith {
      kernels = [ iPython iHaskell ];
    };
in
  jupyterEnvironment.env
