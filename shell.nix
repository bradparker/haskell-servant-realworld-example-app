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
  hlint = packages.hlint;
  hindent = packages.hindent;
  cabal = packages.cabal-install;
  sqitch = nixpkgs.sqitchPg;
  postgresql = nixpkgs.postgresql;
  direnv = nixpkgs.direnv;

  env = (import ./default.nix { inherit nixpkgs; inherit compiler; }).env;
in
  nixpkgs.lib.overrideDerivation env (drv: {
    nativeBuildInputs = drv.nativeBuildInputs ++ [ hlint hindent cabal sqitch postgresql direnv ];
    shellHook = drv.shellHook + "
      mkdir -p $PWD/database/pgdata
      export PGDATA=$PWD/database/pgdata
    ";
  })
