with import <nixpkgs> {};
{ nixpkgs ? import (fetchFromGitHub {
    owner = "NixOs";
    repo = "NixPkgs";
    inherit (builtins.fromJSON (builtins.readFile ./nixpkgs.json)) rev sha256;
  }) {}
, compiler ? "default"
}:
let
  packages = if compiler == "default"
    then nixpkgs.haskellPackages
    else nixpkgs.haskell.packages.${compiler};
  beam-source = fetchFromGitHub {
    owner = "tathougies";
    repo = "beam";
    inherit (builtins.fromJSON (builtins.readFile ./beam.json)) rev sha256;
  };
  beam-core = packages.callPackage (packages.haskellSrc2nix {
    name = "beam-core";
    src = "${beam-source}/beam-core";
  }) {};
  beam-migrate = packages.callPackage (packages.haskellSrc2nix {
    name = "beam-migrate";
    src = "${beam-source}/beam-migrate";
  }) { inherit beam-core; };
  validation-source = fetchFromGitHub {
    owner = "qfpl";
    repo = "validation";
    inherit (builtins.fromJSON (builtins.readFile ./validation.json)) rev sha256;
  };
  validation = packages.callPackage "${validation-source}/validation.nix" {};
in
  packages.callPackage ./package.nix {
    inherit beam-core;
    beam-postgres = packages.callPackage (packages.haskellSrc2nix {
      name = "beam-postgres";
      src = "${beam-source}/beam-postgres";
    }) {
      inherit beam-core;
      inherit beam-migrate;
    };
    inherit validation;
  }
