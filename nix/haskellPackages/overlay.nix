self: super:
  { haskellPackages = super.haskellPackages.override
    { overrides = haskellSelf: haskellSuper:
      { shelly = self.haskell.lib.dontCheck haskellSuper.shelly; };
    };
  }
