{pkgs, ...}: {
  kernel.haskell.hvega = {
    enable = true;
    displayName = "Explore hvega with IHaskell";
    
    # How do I override this to use the local copy of hvega
    # an ihaskell-hvega?
    #
    extraHaskellPackages = ps: [ ps.hvega ps.ihaskell-hvega ps.aeson ps.aeson-pretty
                                 # ps.Frames ps.foldl ps.formatting ps.microlens
				 ];

  };
}
