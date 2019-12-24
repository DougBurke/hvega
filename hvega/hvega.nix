{ mkDerivation, aeson, aeson-pretty, base, bytestring, containers
, filepath, stdenv, tasty, tasty-golden, text, unordered-containers
, vector
}:
mkDerivation {
  pname = "hvega";
  version = "0.4.1.2";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base text unordered-containers vector
  ];
  testHaskellDepends = [
    aeson aeson-pretty base bytestring containers filepath tasty
    tasty-golden text
  ];
  homepage = "https://github.com/DougBurke/hvega";
  description = "Create Vega-Lite visualizations (version 3) in Haskell";
  license = stdenv.lib.licenses.bsd3;
}
