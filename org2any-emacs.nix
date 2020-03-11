with (import <nixpkgs> {});

stdenv.mkDerivation {
  name = "org2any-emacs";
  fileName = "org2any.el";
  version = "0.0.1";
  src = ./org2any.el;
  packagesDir = "emacs-packages";
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
  installPhase = ''
               mkdir -p $out/$packagesDir
               cp $src $out/$packagesDir/$fileName
  '';
}
