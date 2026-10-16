default:
    @just --list

dev: test

test:
    emacs -Q --batch -l tests/bootstrap.el \
        --eval '(mapc (function load) (directory-files (expand-file-name "tests" user-emacs-directory) t "-tests\\.el$"))' \
        -f ert-run-tests-batch-and-exit
