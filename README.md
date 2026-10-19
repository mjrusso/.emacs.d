# mjrusso's .emacs.d

My personalized Emacs configuration. _(Also see: [my system
configurations](https://github.com/mjrusso/nixos-config/).)_

## Packages

Packages are installed via
[straight.el](https://github.com/raxod502/straight.el), in conjunction with
[use-package](https://github.com/jwiegley/use-package), as per
[@jeffkreeftmeijer](https://github.com/jeffkreeftmeijer)'s
[tutorial](https://jeffkreeftmeijer.com/emacs-straight-use-package/).

The completion packages require Emacs 29.1 or newer.

To update a specific package, run `M-x straight-pull-package` (or, `M-x
straight-pull-package-and-deps` to also update all of its dependencies). To
update all packages, use `M-x straight-pull-all`. See [the
documentation](https://github.com/raxod502/straight.el#version-control-operations)
for more options.

After upgrading and testing packages on one machine, run
`M-x straight-freeze-versions` and commit the lockfile with any config changes.

On other machines, pull the configuration, run `just sync-packages` from this
directory, and restart Emacs. Sync applies the lockfile and rebuilds changed
packages. It stops before changing a checkout with local edits.

Startup warns when checkouts differ from the lockfile. Straight applies pins
when cloning repositories; existing checkouts need an explicit sync.

Sync also installs Ghostel's native module in `var/ghostel/` when needed.

## Checks

Run `just ci` for Lisp syntax checks and ERT tests, or run `just lint` and
`just test` separately. Tests load dependencies through Straight without
loading `init.el` or private configuration.

## Thanks

This configuration is heavily adapted from
[@technomancy](https://github.com/technomancy/)'s
[.emacs.d](https://github.com/technomancy/dotfiles/tree/master/.emacs.d), as
well as [emacs-starter-kit
v3](https://github.com/technomancy/emacs-starter-kit/tree/v3) and
[v2](https://github.com/technomancy/emacs-starter-kit/tree/v2).
