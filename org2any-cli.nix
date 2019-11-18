{ mkDerivation, aeson, attoparsec, base, bytestring, containers
, directory, fast-logger, filepath, free, fsnotify, hpack, hspec
, insert-ordered-containers, mtl, optparse-applicative
, orgmode-parse, QuickCheck, random, stdenv, text, thyme, time
, typed-process, unix, universum, unordered-containers, uuid
, stack, Cabal
}:
mkDerivation {
  pname = "org2any-cli";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [ stack ];
  libraryHaskellDepends = [
    aeson attoparsec base bytestring containers directory fast-logger
    filepath free fsnotify insert-ordered-containers mtl
    optparse-applicative orgmode-parse random text thyme time
    typed-process unix universum unordered-containers uuid
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson attoparsec base bytestring containers directory fast-logger
    filepath free fsnotify insert-ordered-containers mtl
    optparse-applicative orgmode-parse random text thyme time
    typed-process unix universum unordered-containers uuid
  ];
  testHaskellDepends = [
    aeson attoparsec base bytestring containers directory fast-logger
    filepath free fsnotify hspec insert-ordered-containers mtl
    optparse-applicative orgmode-parse QuickCheck random text thyme
    time typed-process unix universum unordered-containers uuid
  ];
  prePatch = "hpack";
  homepage = "https://github.com/serhiip/org2any#readme";
  license = stdenv.lib.licenses.bsd3;
}
