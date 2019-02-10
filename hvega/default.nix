{ mkDerivation, aeson, base, stdenv, text, vector }:
mkDerivation {
  pname = "hvega";
  version = "0.2";
  src = ./.;
  libraryHaskellDepends = [ aeson base text vector ];
  homepage = "https://github.com/DougBurke/hvega";
  description = "Create Vega and Vega-Lite visualizations";
  license = stdenv.lib.licenses.bsd3;
}
