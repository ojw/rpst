{ mkDerivation, base, containers, GenericPretty, lens, present
, stdenv, text
}:
mkDerivation {
  pname = "rpst";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base containers GenericPretty lens present text
  ];
  description = "Rock-paper-scissors-inspired tactics game";
  license = stdenv.lib.licenses.bsd3;
}
