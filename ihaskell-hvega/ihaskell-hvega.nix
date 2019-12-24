{ mkDerivation, aeson, base, hvega, ihaskell, stdenv, text }:
mkDerivation {
  pname = "ihaskell-hvega";
  version = "0.2.1.0";
  src = ./.;
  libraryHaskellDepends = [ aeson base hvega ihaskell text ];
  homepage = "https://github.com/DougBurke/hvega";
  description = "IHaskell display instance for hvega types";
  license = stdenv.lib.licenses.bsd3;
}
