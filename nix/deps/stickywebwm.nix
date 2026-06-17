# Checked-in cabal2nix derivation for the stickywebwm engine library (base +
# containers only). Fed `src` explicitly from the flake input, so building
# swwstructor for x86_64-linux carries no import-from-derivation: only the final
# GHC compile runs on the remote builder. The engine source is NOT modified.
{ mkDerivation, base, containers, lib, src }:
mkDerivation {
  pname = "stickywebwm";
  version = "0.1.0.0";
  inherit src;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base containers ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base containers ];
  description = "Sticky Windows — a compositional algebra for responsive tiling layout";
  license = lib.meta.getLicenseFromSpdxId "MIT";
  mainProgram = "stickywebwm-solve";
}
