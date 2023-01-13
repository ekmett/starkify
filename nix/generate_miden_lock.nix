# nix-shell generate_miden_lock.nix
# Generates miden_cargo.lock based on miden source code.
{ tempdir ? toString ./.temp
, output ? toString ./miden_cargo.lock
}:
with (import ./. {});
pkgs.mkShell {
  buildInputs = [ pkgs.xorg.lndir rust ];
  shellHook = ''
    set -e
    mkdir -p "${tmpdir}" && cd "${tmpdir}"
    lndir -silent "${miden.src}" "${tmpdir}"
    cargo generate-lockfile
    mv "${tmpdir}/Cargo.lock" "${output}"
    rm -rf "${tmpdir}"
    exit 0
  '';
}
