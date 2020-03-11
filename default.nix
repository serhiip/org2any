{ nixpkgs ? import (fetchTarball https://github.com/NixOS/nixpkgs/archive/20.03-beta.tar.gz) {} }:
nixpkgs.haskellPackages.callPackage ./org2any-cli.nix { }
