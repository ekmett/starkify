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
  rust = pkgs.rust-bin.stable."1.67.1".default;
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

  starkify = (haskellPackages.callCabal2nix "starkify" (
    pkgs.runCommand "starkify-src" {} ''
      mkdir -p $out
      cp -R ${../src} $out/src
      cp -R ${../app} $out/app
      cp -R ${../tests} $out/tests
      cp ${../starkify.cabal} $out
    ''
  ) {}).overrideAttrs (old: { doCheck = false; });

  ghc = haskellPackages.ghcWithPackages (p: (
    starkify.getCabalDeps.executableHaskellDepends
    ++ starkify.getCabalDeps.libraryHaskellDepends
    ++ starkify.getCabalDeps.testHaskellDepends)
  );

  python = pkgs.python39;
  web3-fixed = python.pkgs.web3.override {
    # TODO: Check how IPFS is used, and whether it works on macOS
    ipfshttpclient = python.pkgs.ipfshttpclient.overridePythonAttrs {
      meta.broken = false;
    };
  };
  cairo-lang = python.pkgs.buildPythonPackage {
    pname = "cairo-lang";
    src = pkgs.fetchzip {
      url = "https://github.com/starkware-libs/cairo-lang/releases/download/v${lock.cairo-lang.version}/cairo-lang-${lock.cairo-lang.version}.zip";
      sha256 = lock.cairo-lang.sha256;
    };
    version = lock.cairo-lang.version;
    nativeBuildInputs = [ python.pkgs.pythonRelaxDepsHook ];
    pythonRelaxDeps = [ "frozendict" ];
    pythonRemoveDeps = [ "pytest" "pytest-asyncio" ];
    doCheck = false;
    buildInputs = [ pkgs.gmp ];
    propagatedBuildInputs = with python.pkgs; ([
      aiohttp cachetools setuptools ecdsa fastecdsa sympy mpmath
      numpy typeguard frozendict prometheus-client marshmallow
      marshmallow-enum marshmallow-dataclass marshmallow-oneofschema
      pipdeptree lark eth-hash pyyaml web3-fixed
    ] ++ eth-hash.optional-dependencies.pycryptodome);
    postInstall = ''
      chmod +x $out/bin/*
    '';
  };

in
{ inherit pkgs rust miden haskellPackages ghc starkify cairo-lang; }
