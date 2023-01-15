# Updating packages
If you need to update nixpkgs, rust-overlay or miden version, update `commit` in `default.lock.toml`, and change the respective `sha256` to be empty. The correct hash will be printed out by nix when you try to build the resources, which you can then paste into `default.lock.toml`

# Updating the miden lockfile
When bumping the miden source version, you might also need to generate a new lockfile. To simplify this step, you can use generate_miden_lock.nix as a script with nix-shell (this is a bit of a hack)
```
nix-shell generate_miden_lock.nix
```

# Docker image from nix
Loading the image requires that you have docker installed. The building itself does not require docker, and produces a .tar.gz archive.
```
docker load < $(nix-build docker.nix)
```


