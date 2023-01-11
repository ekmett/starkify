with (import ./. {});
pkgs.mkShell {
  name = "starkify";
  buildInputs = [
    pkgs.llvmPackages_14.clang
    pkgs.llvmPackages_14.libllvm
    pkgs.lld_14
    pkgs.cabal-install
    pkgs.ghc
    pkgs.wabt
    pkgs.wasmtime
    
    rust
    miden
  ];
}
