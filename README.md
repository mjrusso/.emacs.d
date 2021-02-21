# mjrusso's .emacs.d

My personalized Emacs configuration. _([other dotfiles](https://github.com/mjrusso/dotfiles/))_

## packages

The majority of Emacs packages are vendored into `lib/`.

To add a new vendored package, use
[git subtree](https://www.atlassian.com/git/tutorials/git-subtree):

```bash
git subtree add --prefix lib/<package-name> <git-repository-url>  master --squash
```

To byte-compile and autoload the vendored packages, use `M-x pnh-reinit-libs`.

(Note that some packages, like [magit](https://magit.vc),
[company-mode](https://company-mode.github.io),
[lsp-mode](https://emacs-lsp.github.io/lsp-mode/),
[org-roam](https://github.com/org-roam/org-roam), and
[org-download](https://github.com/abo-abo/org-download) are installed via
[MELPA](https://melpa.org/).)

## themes

Themes are vendored into `themes/`.

To add a new theme:

```bash
git subtree add --prefix themes/<package-name> <git-repository-url>  master --squash
```

## thanks

This configuration is heavily adapted from
[@technomancy](https://github.com/technomancy/)'s
[.emacs.d](https://github.com/technomancy/dotfiles/tree/master/.emacs.d), as
well as [emacs-starter-kit v3](https://github.com/technomancy/emacs-starter-kit/tree/v3)
and [v2](https://github.com/technomancy/emacs-starter-kit/tree/v2).
