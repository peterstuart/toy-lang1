{ mkDerivation, base, bytestring, filepath, megaparsec
, parser-combinators, stdenv, tasty, tasty-golden
}:
mkDerivation {
  pname = "toy-lang1";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base megaparsec parser-combinators ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base bytestring filepath tasty tasty-golden
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
