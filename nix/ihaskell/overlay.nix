self: super:
  { ihaskellWithPackages = ps:
      self.callPackage
        "${self.path}/pkgs/development/tools/haskell/ihaskell/wrapper.nix"
        {
          ghcWithPackages = self.haskellPackages.ghcWithPackages;
          jupyter = self.python3.withPackages (ps: [ ps.jupyter ps.notebook ]);
          packages = ps;
        };
  }
