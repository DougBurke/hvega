{ mkDerivation, aeson, aeson-pretty, base, bytestring, containers
, directory, filepath, http-conduit, stdenv, tagsoup, tasty
, tasty-golden, text, unordered-containers
}:
mkDerivation {
  pname = "hvega";
  version = "0.11.0.0";
  src = ./.;
  configureFlags = [ "-ftools" ];
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ aeson base text unordered-containers ];
  executableHaskellDepends = [
    aeson aeson-pretty base bytestring directory filepath http-conduit
    tagsoup text
  ];
  testHaskellDepends = [
    aeson aeson-pretty base bytestring containers filepath tasty
    tasty-golden text unordered-containers
  ];
  homepage = "https://github.com/DougBurke/hvega";
  description = "Create Vega-Lite visualizations (version 4) in Haskell";
  license = stdenv.lib.licenses.bsd3;
}
