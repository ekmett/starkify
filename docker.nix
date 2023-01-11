with (import ./. { system = "aarch64-linux"; });
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
    (pkgs.runCommand "starkify-env" {} ''
      mkdir -p $out/bin
      cp ${starkify}/bin/starkify $out/bin/starkify
      cp ${miden}/bin/miden $out/bin/miden
    '')
  ]
  # Not needed for image to function, but useful for debugging from a shell
  ++ (with pkgs; [ coreutils which gnugrep vim ]);
}
