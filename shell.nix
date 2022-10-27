{ pkgs ? import <nixpkgs> {
    overlays = [
      (import (fetchTarball "https://github.com/oxalica/rust-overlay/archive/master.tar.gz"))
    ];
  }
}:

with pkgs;
let llvmPkgs = [ llvmPackages_14.clang llvmPackages_14.libllvm lld_14 ];
    rust162 = rust-bin.stable."1.62.0".default;
    miden = rustPlatform.buildRustPackage {
      pname = "miden";
      version = "30afebad83fae8651f25d65e739c907b94d678d9";
      src = fetchFromGitHub {
        owner = "maticnetwork";
        repo = "miden";
        rev = "fea07e42094dcb0f1f0835507e6fbe05f9f09158";
        hash = "sha256-ueJRw9zcjOwkdh3vivhjJOvjHl0ZhDEMAx0ow8Oz9DM=";
      };
      cargoPatches = [
        ./miden_cargolock.patch
      ];
      cargoLock = {
        lockFile = ./miden_cargo.lock;
      };
      cargoSha256 = lib.fakeSha256;
      buildType = "release";
      buildFeatures = [ "executable" "concurrent" ];
      nativeBuildInputs = [ rust162 ];
      doCheck = false;
    };
in

mkShell {
  name = "wasm-checker";
  buildInputs = [
    cabal-install haskell.compiler.ghc924
    wabt
    rust162 cargo miden
  ] ++ llvmPkgs;
}
