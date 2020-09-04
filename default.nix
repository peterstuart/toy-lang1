{ mkDerivation, base, megaparsec, parser-combinators, stdenv }:
mkDerivation {
  pname = "toy-lang1";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base megaparsec parser-combinators ];
  executableHaskellDepends = [ base ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
