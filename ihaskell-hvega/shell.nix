{ compiler ? "ghc928"
}:
(import ./default.nix { compiler = compiler; }).shell
