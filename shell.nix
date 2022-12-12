{ pkgs ? import (fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/refs/tags/22.11.tar.gz";
      sha256 = "sha256:11w3wn2yjhaa5pv20gbfbirvjq6i3m7pqrq2msf0g7cv44vijwgw";
    }) {
    overlays = [
      (import (fetchTarball {
        url = "https://github.com/oxalica/rust-overlay/archive/fe185fac76e009b4bd543704a8c61077cf70155b.tar.gz";
        sha256 = "sha256:1ipbr07gxdp7q15vavskypqc1faspz7y7f0sygy88xr7i8p0mls5";
      }))
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
    haskellPackages = haskell.packages.ghc924.override {
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
in

mkShell {
  name = "wasm-checker";
  buildInputs = [
    cabal-install ghc
    wabt
    rust162 cargo miden
  ] ++ llvmPkgs;
}
