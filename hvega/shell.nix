{ compiler ? "ghc8107"
}:
(import ./default.nix { compiler = compiler; }).shell
