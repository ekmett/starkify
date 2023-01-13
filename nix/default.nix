{ system ? builtins.currentSystem
, lock ? builtins.fromTOML (builtins.readFile ./default.lock.toml)
, pkgs ? import (fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${lock.nixpkgs.commit}.tar.gz";
    sha256 = lock.nixpkgs.sha256;
  }) { inherit system overlays; }
, overlays ? [
    (import (fetchTarball {
      url = "https://github.com/oxalica/rust-overlay/archive/${lock.rust-overlay.commit}.tar.gz";
      sha256 = lock.rust-overlay.sha256;
    }))
  ]
}:
let
  rust = pkgs.rust-bin.stable."1.66.0".default;
  miden = pkgs.rustPlatform.buildRustPackage rec {
    pname = "miden";
    version = lock.miden-vm.commit;
    src = pkgs.fetchFromGitHub {
      owner = "0xPolygonMiden";
      repo = "miden-vm";
      rev = lock.miden-vm.commit;
      sha256 = lock.miden-vm.sha256;
    };
    cargoLock.lockFile = ./miden_cargo.lock;
    cargoPatches = [
      (pkgs.runCommand "miden_cargolock.patch" { buildInputs = [ pkgs.git ]; } ''
        cp "${./miden_cargo.lock}" Cargo.lock
        git init && git add Cargo.lock
        git diff --cached > $out
      '')
    ];
    buildType = "release";
    buildFeatures = [ "executable" "concurrent" ];
    nativeBuildInputs = [ rust ];
    doCheck = false;
  };
  haskellPackages = pkgs.haskell.packages.ghc924.override {
    overrides = self: super: {
      wasm = pkgs.haskell.lib.dontCheck (self.callHackage "wasm" "1.1.1" {});
    };
  };
  ghc = haskellPackages.ghcWithPackages (p: with p; [
    aeson
    monad-validate
    optparse-applicative
    pretty-simple
    temporary
    wasm
  ]);
  starkify = with haskellPackages; mkDerivation {
    pname = "starkify";
    version = "0.1.0.0";
    mainProgram = "starkify";
    isExecutable = true;
    doCheck = false;
    license = "unknown";
    src = pkgs.runCommand "starkify-src" {} ''
      mkdir -p $out
      cp -R ${../src} $out/src
      cp -R ${../app} $out/app
      cp -R ${../tests} $out/tests
      cp ${../starkify.cabal} $out
    '';
    executableHaskellDepends = [
      aeson base bytestring containers directory dlist filepath
      monad-validate mtl optparse-applicative pretty-simple process
      temporary text vector wasm
    ];
  };
in
{ inherit pkgs rust miden haskellPackages ghc starkify; }
