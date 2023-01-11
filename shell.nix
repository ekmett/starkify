{ pkgs ? import (fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/refs/tags/22.11.tar.gz";
      sha256 = "sha256:11w3wn2yjhaa5pv20gbfbirvjq6i3m7pqrq2msf0g7cv44vijwgw";
    }) {
    overlays = [
      (import (fetchTarball {
        url = "https://github.com/oxalica/rust-overlay/archive/ede977678e5d0164316998487e686d0790744cd7.tar.gz";
        sha256 = "sha256:05sk61p3a5dhqfiaz7w01fc5fphxhl9d1aizwbvgiw7g0hrbc4v8";
      }))
    ];
  }
}:

with pkgs;
let llvmPkgs = [ llvmPackages_14.clang llvmPackages_14.libllvm lld_14 ];
    rust166 = rust-bin.stable."1.66.0".default;
    miden = rustPlatform.buildRustPackage rec {
      pname = "miden";
      version = "91590aefd92c0185c7a9593fec5eae4d64d1088c";
      src = fetchFromGitHub {
        owner = "0xPolygonMiden";
        repo = "miden-vm";
        rev = version;
        sha256 = "sha256-TUwnZJm9zsnC4/M/z2ZMw7jjEK/cpCJDVOE0I6+AkI0=";
      };
      cargoPatches = [ ./miden_cargolock.patch ];
      cargoLock.lockFile = ./miden_cargo.lock;
      buildType = "release";
      buildFeatures = [ "executable" "concurrent" ];
      nativeBuildInputs = [ rust166 ];
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
  name = "starkify";
  buildInputs = [
    cabal-install ghc
    wabt wasmtime
    rust166 cargo miden
  ] ++ llvmPkgs;
}
