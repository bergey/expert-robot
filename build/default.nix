{ mkDerivation, alex, array, base, bytestring, stdenv, text }:
mkDerivation {
  pname = "expert-robot";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ array base bytestring text ];
  executableToolDepends = [ alex ];
  homepage = "https://github.com/bergey/expert-robot#readme";
  license = stdenv.lib.licenses.bsd3;
}
