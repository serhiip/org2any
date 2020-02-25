{ mkDerivation, aeson, attoparsec, bytestring, containers
, directory, fast-logger, filepath, free, fsnotify, hpack, hspec
, insert-ordered-containers, mtl, optparse-applicative
, orgmode-parse, QuickCheck, random, stdenv, text, thyme, time
, typed-process, unix, universum, unordered-containers, uuid, base
}:
mkDerivation {
  pname = "org2any-cli";
  version = "0.0.1";
  src = ./.;
  isLibrary = false;
  doCheck = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson attoparsec base bytestring containers directory fast-logger
    filepath free fsnotify insert-ordered-containers mtl
    orgmode-parse random text thyme time typed-process
    unordered-containers uuid universum
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [ optparse-applicative unix ];
  testHaskellDepends = [ hspec QuickCheck ];
  prePatch = "hpack";
  homepage = "https://github.com/serhiip/org2any#readme";
  license = stdenv.lib.licenses.bsd3;
}
