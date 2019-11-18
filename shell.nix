{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;

  org2any-emacs = import ./org2any-emacs.nix;

  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: {
      org2any-cli = self.callPackage ./org2any-cli.nix {};
    };
  };

  haskellDeps = ps: with ps; [
    base
    org2any-cli
  ];

  ghc = haskellPackages.ghcWithPackages haskellDeps;
in
pkgs.stdenv.mkDerivation {
  name = "env";
  buildInputs = [
    ghc
    org2any-emacs
    pkgs.emacs
    pkgs.emacsPackages.dash
    haskellPackages.org2any-cli
    haskellPackages.stack
    haskellPackages.cabal-install
  ];
}
