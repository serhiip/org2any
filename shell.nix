{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;
  org2any-emacs = import ./org2any-emacs.nix;
in
pkgs.stdenv.mkDerivation {
  name = "dev-env";
  buildInputs = [
    pkgs.haskellPackages.ghc
    pkgs.haskellPackages.stack
    pkgs.haskellPackages.cabal-install
    org2any-emacs
    pkgs.emacs
    pkgs.emacsPackages.dash
  ];
}
