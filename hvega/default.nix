{ mkDerivation, aeson, base, stdenv, text, vector }:
mkDerivation {
  pname = "hvega";
  version = "0.3.0.0";
  src = ./.;
  libraryHaskellDepends = [ aeson base text vector ];
  homepage = "https://github.com/DougBurke/hvega";
  description = "Create Vega-Lite visualizations in Haskell";
  license = stdenv.lib.licenses.bsd3;
}
