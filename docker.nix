# Should we create an API for starkify?
# (might make it simpler for blockchain team to use it as a service)

{ system ? "aarch64-linux" # Change to x86_64-linux?
, pkgs ? import (fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/refs/tags/22.11.tar.gz";
    sha256 = "sha256:11w3wn2yjhaa5pv20gbfbirvjq6i3m7pqrq2msf0g7cv44vijwgw";
  }) {
    system = system;
    overlays = [
      (import (fetchTarball {
        url = "https://github.com/oxalica/rust-overlay/archive/ede977678e5d0164316998487e686d0790744cd7.tar.gz";
        sha256 = "sha256:05sk61p3a5dhqfiaz7w01fc5fphxhl9d1aizwbvgiw7g0hrbc4v8";
      }))
    ];
  }
}:

let
  rust166 = pkgs.rust-bin.stable."1.66.0".default;
  miden = pkgs.rustPlatform.buildRustPackage rec {
    pname = "miden";
    version = "a95e9c30376646669b0353b55739edc4cc520d29";
    src = pkgs.fetchFromGitHub {
      owner = "0xPolygonMiden";
      repo = "miden-vm";
      rev = version;
      sha256 = "sha256-hXKFmCiBJ2JT1RMtNe4gS7gHdd7OE6hdVPyG5BjSv1U=";
    };
    cargoPatches = [ ./miden_cargolock.patch ];
    cargoLock.lockFile = ./miden_cargo.lock;
    buildType = "release";
    buildFeatures = [ "executable" "concurrent" ];
    nativeBuildInputs = [ rust166 ];
    doCheck = false;
  };
  haskellPackages = pkgs.haskell.packages.ghc924.override {
    overrides = self: super: {
      wasm = pkgs.haskell.lib.dontCheck (self.callHackage "wasm" "1.1.1" {});
    };
  };
  starkify = with haskellPackages; mkDerivation {
    pname = "starkify";
    version = "0.1.0.0";
    mainProgram = "starkify";
    isExecutable = true;
    doCheck = false;
    license = "unknown";
    src = pkgs.runCommand "starkify-src" {} ''
      mkdir -p $out
      cp -R ${./src} $out/src
      cp -R ${./app} $out/app
      cp -R ${./tests} $out/tests
      cp ${./starkify.cabal} $out
    '';
    executableHaskellDepends = [
      aeson base bytestring containers directory dlist filepath
      monad-validate mtl optparse-applicative pretty-simple process
      temporary text vector wasm
    ];
  };
  starkify-env = pkgs.runCommand "starkify-env" {} ''
    mkdir -p $out/bin
    cp ${starkify}/bin/starkify $out/bin/starkify
    cp ${miden}/bin/miden $out/bin/miden
  '';
in
pkgs.dockerTools.buildImage {
  name = "starkify";
  tag = "latest";
  created = "now";
  fromImage = pkgs.dockerTools.buildImage {
    name = "starkify-base";
    fromImageName = "scratch";
    copyToRoot = [
      pkgs.dockerTools.usrBinEnv
      pkgs.dockerTools.binSh
      pkgs.dockerTools.caCertificates
      pkgs.dockerTools.fakeNss
      # Ensure that /tmp exists, since this is used for clang-14 output
      (pkgs.symlinkJoin {
        name = "tmpdir";
        paths = [(pkgs.runCommand "" {} "mkdir -p $out/tmp")];
      })
      # LLVM packages adds ~1.3GB to the docker image
      pkgs.llvmPackages_14.clang.cc
      pkgs.llvmPackages_14.libllvm
      pkgs.lld_14
    ];
  };
  copyToRoot = [
    starkify-env
  ]
  # Not needed for image to function, but useful for debugging from a shell
  ++ (with pkgs; [ coreutils which gnugrep vim ]);
}
