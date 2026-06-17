# Checked-in cabal2nix derivation for swwstructor (library + server executable +
# test-suite). `src` and `stickywebwm` are supplied by the flake. The test-suite
# (the NYT benchmark) runs during the check phase, so a successful build implies
# 55/55 green.
{ mkDerivation, base, bytestring, containers, crypton, directory
, filepath, HsYAML, http-client, http-client-tls, http-types, lib
, memory, mtl, scotty, stickywebwm, text, wai-extra, warp, src
}:
mkDerivation {
  pname = "swwstructor";
  version = "0.1.0.0";
  inherit src;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base bytestring containers mtl stickywebwm text ];
  executableHaskellDepends = [
    base bytestring containers crypton directory filepath HsYAML
    http-client http-client-tls http-types memory mtl scotty stickywebwm
    text wai-extra warp
  ];
  testHaskellDepends = [ base containers stickywebwm text ];
  description = "A single-tenant website constructor on the stickywebwm layout engine";
  license = lib.meta.getLicenseFromSpdxId "MIT";
  mainProgram = "swwstructor-server";
}
