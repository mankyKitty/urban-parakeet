{ mkDerivation, base, bytestring, containers, lens, linear, mtl
, reactive-banana, stdenv, text, vty
}:
mkDerivation {
  pname = "rb-vty";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring containers lens linear mtl reactive-banana text vty
  ];
  license = stdenv.lib.licenses.bsd3;
}
