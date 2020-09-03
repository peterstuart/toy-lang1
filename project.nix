{ mkDerivation, base, megaparsec, parser-combinators, stdenv }:
mkDerivation {
  pname = "toy-lang1";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base megaparsec parser-combinators ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
