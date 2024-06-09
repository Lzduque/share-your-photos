{ mkDerivation
, base
, lib
, text
, webdriver
, zlib
, scotty
, scalpel
}:

mkDerivation {
  pname = "share-your-photos";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends =[
    base
    text
    webdriver
    zlib
    scotty
    scalpel
  ];
  license = lib.licenses.bsd3;
}
