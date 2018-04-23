let
  pkgs = ./nixpkgs ;
  fetchOverlay = self: super:
    { fetch = path:
        let
          dropBranch = {...}@attrs:
            { inherit (attrs) rev sha256 owner repo; };
          fullSpec = (builtins.fromJSON (builtins.readFile path));
        in self.fetchFromGitHub (dropBranch fullSpec);
    };

  extra =
    { overlays =
        [
          fetchOverlay
          (import ./haskellPackages/overlay.nix)
          (import ./ihaskell/overlay.nix)
        ];
    };
in import pkgs extra
