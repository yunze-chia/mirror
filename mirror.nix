{ mkDerivation, base, bytestring, cassava, containers, lib
, optparse-applicative, time
}:
mkDerivation {
  pname = "mirror";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base cassava containers time ];
  executableHaskellDepends = [
    base bytestring cassava optparse-applicative time
  ];
  testHaskellDepends = [ base ];
  license = "unknown";
  mainProgram = "mirror";
}
