# mjrusso's .emacs.d

My personalized Emacs configuration. _([other dotfiles](https://github.com/mjrusso/dotfiles/))_

## packages

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

## thanks

This configuration is heavily adapted from
[@technomancy](https://github.com/technomancy/)'s
[.emacs.d](https://github.com/technomancy/dotfiles/tree/master/.emacs.d), as
well as [emacs-starter-kit
v3](https://github.com/technomancy/emacs-starter-kit/tree/v3) and
[v2](https://github.com/technomancy/emacs-starter-kit/tree/v2).
