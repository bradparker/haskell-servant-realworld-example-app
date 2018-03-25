{ mkDerivation, aeson, aeson-casing, base, beam-core, beam-postgres
, bytestring, conduit, containers, data-default, errors
, generic-lens, hspec, hspec-core, hspec-wai, hspec-wai-json
, http-types, jwt, lens, lens-aeson, mtl, postgresql-simple
, resource-pool, scrypt, servant, servant-server, sqlite-simple
, stdenv, text, time, transformers, validation, vector, wai
, wai-extra, warp
}:
mkDerivation {
  pname = "haskell-servant-realworld-example-app";
  version = "0.0.1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-casing base beam-core beam-postgres bytestring conduit
    containers data-default errors generic-lens jwt lens mtl
    postgresql-simple resource-pool scrypt servant servant-server text
    time transformers validation wai
  ];
  executableHaskellDepends = [
    aeson base bytestring containers jwt mtl servant servant-server
    sqlite-simple text time wai warp
  ];
  testHaskellDepends = [
    base beam-core bytestring containers hspec hspec-core hspec-wai
    hspec-wai-json http-types jwt lens lens-aeson postgresql-simple
    resource-pool scrypt servant-server text vector wai wai-extra
  ];
  homepage = "https://github.com/dorlowd/haskell-servant-realworld-example-app#readme";
  license = stdenv.lib.licenses.bsd3;
}
