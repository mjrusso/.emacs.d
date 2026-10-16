# mjrusso's .emacs.d

My personalized Emacs configuration. _(Also see: [my system
configurations](https://github.com/mjrusso/nixos-config/).)_

## Packages

Packages are installed via
[straight.el](https://github.com/raxod502/straight.el), in conjunction with
[use-package](https://github.com/jwiegley/use-package), as per
[@jeffkreeftmeijer](https://github.com/jeffkreeftmeijer)'s
[tutorial](https://jeffkreeftmeijer.com/emacs-straight-use-package/).

To update a specific package, run `M-x straight-pull-package` (or, `M-x
straight-pull-package-and-deps` to also update all of its dependencies). To
update all packages, use `M-x straight-pull-all`. See [the
documentation](https://github.com/raxod502/straight.el#version-control-operations)
for more options.

To update the package version lockfile, run `M-x straight-freeze-versions`.
(Likewise, to revert all packages to the revisions specified in the lockfile,
run `M-x straight-thaw-versions`.)

## Checks

Run all tests from this directory:

```sh
just test
```

This loads every `tests/*-tests.el` file and runs the tests with Emacs's
built-in ERT framework. The test bootstrap uses the local Straight installation
to load the module's dependencies; it does not load `init.el` or private
configuration.

Run `just lint` to check Lisp syntax in tracked and unignored new `.el` files.
It checks parentheses, strings, and readable Lisp forms without loading the
configuration or creating compiled files.

Run `just ci` to lint and test.

## Thanks

This configuration is heavily adapted from
[@technomancy](https://github.com/technomancy/)'s
[.emacs.d](https://github.com/technomancy/dotfiles/tree/master/.emacs.d), as
well as [emacs-starter-kit
v3](https://github.com/technomancy/emacs-starter-kit/tree/v3) and
[v2](https://github.com/technomancy/emacs-starter-kit/tree/v2).
