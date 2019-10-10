with (import <nixpkgs> {});

stdenv.mkDerivation {
  name = "org2any";
  version = "0.0.1";
  src = ./org2any.el;
  unpackPhase = ":";
  buildInputs = [ pkgs.emacs pkgs.emacsPackages.dash ];
  buildPhase = ''
             echo '(setenv "HOME" "'$TMP'") (package-initialize)' > init.el
             emacs --batch --script init.el --eval '(progn
                                                      (package-install-file "'$src'")
                                                      (load-file "'$src'"))'
  '';
  checkInputs = [ pkgs.emacs pkgs.emacsPackages.dash ];
  doCheck = true;
  checkPhase = "emacs --batch --script init.el -l ert -l $src -f ert-run-tests-batch-and-exit";
  installPhase = "mkdir -p $out";
}
