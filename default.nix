{ nixpkgs ? import <nixpkgs> {} }:
nixpkgs.haskellPackages.callPackage ./org2any-cli.nix { }
