{ mkDerivation, aeson, aeson-pretty, base, bytestring, stdenv
, tasty, tasty-golden, text, vector
}:
mkDerivation {
  pname = "hvega";
  version = "0.4.0.0";
  src = ./.;
  libraryHaskellDepends = [ aeson base text vector ];
  testHaskellDepends = [
    aeson-pretty base bytestring tasty tasty-golden
  ];
  homepage = "https://github.com/DougBurke/hvega";
  description = "Create Vega-Lite visualizations (version 3) in Haskell";
  license = stdenv.lib.licenses.bsd3;
}
