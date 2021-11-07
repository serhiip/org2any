#!/bin/sh -e

# adapted from https://github.com/purcell/package-lint/blob/master/run-tests.sh

EMACS="${EMACS:=emacs}"

NEEDED_PACKAGES="dash package-lint"

INIT_PACKAGE_EL="(progn \
  (require 'package) \
  (push '(\"melpa\" . \"https://melpa.org/packages/\") package-archives) \
  (package-initialize) \
  (unless package-archive-contents \
     (package-refresh-contents)) \
  (dolist (pkg '(${NEEDED_PACKAGES})) \
    (unless (package-installed-p pkg) \
      (package-install pkg))))"

# Refresh package archives, because the test suite needs to see at least
# dash.
"$EMACS" -Q -batch \
         --eval "$INIT_PACKAGE_EL"

# Byte compile, failing on byte compiler errors, or on warnings unless ignored
ERROR_ON_WARN=nil

"$EMACS" -Q -batch \
         --eval "$INIT_PACKAGE_EL" \
         --eval "(setq byte-compile-error-on-warn ${ERROR_ON_WARN})" \
         -f batch-byte-compile \
         org2any.el
# Lint
# "$EMACS" -Q -batch \
#          --eval "$INIT_PACKAGE_EL" \
#          -L . \
#          --eval "(require 'package-lint)" \
#          -f package-lint-batch-and-exit \
#          org2any.el || [ -n "${EMACS_LINT_IGNORE+x}" ]
# Finally, run the testsuite
"$EMACS" -Q -batch \
         --eval "$INIT_PACKAGE_EL" \
         -l org2any.elc \
         -f ert-run-tests-batch-and-exit
