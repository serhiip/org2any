{ nixpkgs ? import (fetchTarball https://github.com/NixOS/nixpkgs/archive/20.03-beta.tar.gz) {} }:
let
  inherit (nixpkgs) pkgs;
  org2any-cli = pkgs.haskellPackages.callPackage ./org2any-cli.nix { };
  org2any-emacs = import ./org2any-emacs.nix;
in
pkgs.stdenv.mkDerivation {
  name = "dev-env";
  buildInputs = org2any-emacs.buildInputs ++
                [
                  pkgs.haskellPackages.ghc
                  pkgs.haskellPackages.cabal-install
                  pkgs.which
                  org2any-emacs
                  org2any-cli
                ];
}
