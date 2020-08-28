{ mkDerivation, base, parsec, stdenv }:
mkDerivation {
  pname = "haskell-test";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base parsec ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
