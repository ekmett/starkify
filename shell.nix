with (import ./nix {});
pkgs.mkShell {
  name = "starkify";
  buildInputs = [
    pkgs.llvmPackages_14.clang
    pkgs.llvmPackages_14.libllvm
    pkgs.lld_14
    pkgs.cabal-install
    pkgs.wabt
    pkgs.wasmtime

    miden
    rust
    ghc
  ];
}
