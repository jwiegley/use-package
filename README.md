# Note to users upgrading to 2.0

The meaning of `:init` has been changed: It now *always* happens before
package load, whether `:config` has been deferred or not.  This means that
some uses of `:init` in your configuration may need to be changed to `:config`
(in the non-deferred case).  For the deferred case, the behavior is unchanged
from before.

# `use-package`

The `use-package` macro allows you to isolate package configuration in your
`.emacs` file in a way that is both performance-oriented and, well, tidy.  I
created it because I have over 80 packages that I use in Emacs, and things
were getting difficult to manage.  Yet with this utility my total load time is
around 2 seconds, with no loss of functionality!

## The basics

Here is the simplest `use-package` declaration:

``` elisp
(use-package foo)
```

This loads in the package `foo`, but only if `foo` is available on your
system.  If not, a warning is logged to the `*Messages*` buffer.  If it
succeeds, a message about `"Loading foo"` is logged, along with the time it
took to load, if it took over 0.1s.

Use the `:init` keyword to execute code before a package is loaded.  It
accepts one or more form, up until the next keyword:

``` elisp
(use-package foo
  :init
  (setq foo-variable t))
```

Similarly, `:config` can be used to execute code after a package is loaded.
In cases where loading is done lazily (see more about autoloading below), this
execution is deferred until after the autoload occurs:

``` elisp
(use-package foo
  :init
  (setq foo-variable t)
  :config
  (foo-mode 1))
```

As you might expect, you can use `:init` and `:config` together:

``` elisp
(use-package color-moccur
  :commands (isearch-moccur isearch-all)
  :bind ("M-s O" . moccur)
  :init
  (bind-key "M-o" 'isearch-moccur isearch-mode-map)
  (bind-key "M-O" 'isearch-moccur-all isearch-mode-map)
  :config
  (use-package moccur-edit))
```

In this case, I want to autoload the commands `isearch-moccur` and
`isearch-all` from `color-moccur.el`, and bind keys both at the global level
and within the `isearch-mode-map` (see next section).  When the package is
actually loaded (by using one of these commands), `moccur-edit` is also be
loaded, to allow editing of the `moccur` buffer.

## Key-binding

Another common thing to do when loading a module is to bind a key to primary
commands within that module:

``` elisp
(use-package ace-jump-mode
  :bind ("C-." . ace-jump-mode))
```

This does two things: first, it creates an autoload for the `ace-jump-mode`
command and defers loading of `ace-jump-mode` until you actually use it.
Second, it binds the key `C-.` to that command.  After loading, you can use
`M-x describe-personal-keybindings` to see all such keybindings you've set
throughout your `.emacs` file.

A more literal way to do the exact same thing is:

``` elisp
(use-package ace-jump-mode
  :commands ace-jump-mode
  :init
  (bind-key "C-." 'ace-jump-mode))
```

When you use the `:commands` keyword, it creates autoloads for those commands
and defers loading of the module until they are used.  Since the `:init` form
is always run -- even if `ace-jump-mode` might not be on your system --
remember to restrict `:init` code to only what would succeed either way.

The `:bind` keyword takes either a cons or a list of conses:

``` elisp
(use-package hi-lock
  :bind (("M-o l" . highlight-lines-matching-regexp)
         ("M-o r" . highlight-regexp)
         ("M-o w" . highlight-phrase)))
```

The `:commands` keyword likewise takes either a symbol or a list of symbols.

## Modes and interpreters

Similar to `:bind`, you can use `:mode` and `:interpreter` to establish a
deferred binding within the `auto-mode-alist` and `interpreter-mode-alist`
variables.  The specifier to either keyword can be a cons cell, a list, or
just a string:

``` elisp
(use-package ruby-mode
  :mode "\\.rb\\'"
  :interpreter "ruby")

;; The package is "python" but the mode is "python-mode":
(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode))
```

If you aren't using `:commands`, `:bind`, `:bind*`, `:bind-keymap`,
`:bind-keymap*`, `:mode`, or `:interpreter` (all of which imply `:commands`;
see the docstring for `use-package` for a brief description of each), you can
still defer loading with the `:defer` keyword:

``` elisp
(use-package ace-jump-mode
  :defer t
  :init
  (autoload 'ace-jump-mode "ace-jump-mode" nil t)
  (bind-key "C-." 'ace-jump-mode))
```

This does exactly the same thing as the other two commands above.

## Overriding lazy loading

You can override package deferral with the `:demand` keyword.  Thus, even if
you use `:bind`, using `:demand` will force loading to occur immediately and
not establish an autoload for the bound key.

## Information about package loading

When a package is loaded, and if you have `use-package-verbose` set t or if
the package takes longer than 0.1s to load, you will see a message to indicate
this loading activity in the `*Messages*` buffer.  The same will happen for
configuration, or `:config` blocks that take longer than 0.1s to execute.  In
general, you should keep `:init` forms as simple and quick as possible, and
put as much as you can get away with into the `:config` block.  This way,
deferred loading can help your Emacs to start as quickly as possible.

Additionally, if an error occurs while initializing or configuring a package,
this will not stop your Emacs from loading.  Rather, the error will be
captured by `use-package`, and reported to a special `*Warnings*` popup
buffer, so that you can debug the situation in an otherwise functional Emacs.

## Idle loads

Another similar option to `:init` and `:config` is `:idle`.  Like `:init`,
this form is always run, however, it does so when Emacs has been idle for some
length of time after loading your Emacs (see the variable
`use-package-idle-interval`).  This is particularly useful for convienience
minor modes which can be slow to load.

For example, consider the following package declaration for `pabbrev`:
`:commands` creates an appropriate autoload, and defers loading of the package
until that command has been used; `:idle` run this command at some point after
Emacs has finished loading.  But if you start Emacs and start typing straight
away, loading will happen immediately due to the autoload for
`global-pabbrev-mode`.  This way, `pabbrev` is either loaded on-demand, or in
the background if you leave Emacs idle.

``` elisp
(use-package pabbrev
  :commands global-pabbrev-mode
  :idle (global-pabbrev-mode)
  :idle-priority 3)
```

Idle functions are run in the order in which they are evaluated, unless you
specify a priority using `:idle-priority`.  Lower priority functions are run
first (the default is 5).  If you have many idle functions, it may take
sometime for all of them to run.  `use-package` will always indicate if there
is an error in the form.  It may even tell you about the functions being
eval'd, depending on the value of `use-package-verbose` (and so, enabling this
can be helpful for debugging).  Other good candidates for `:idle` are
`yasnippet`, `auto-complete` and `autopair`.

## Conditional loading

You can use the `:if` keyword to predicate the loading and initialization of
modules.  For example, I only want `edit-server` running for my main,
graphical Emacs, not for other Emacsen I may start at the command line:

``` elisp
(use-package edit-server
  :if window-system
  :init
  (add-hook 'after-init-hook 'server-start t)
  (add-hook 'after-init-hook 'edit-server-start t))
```

The `:disabled` keyword can turn off a module you're having difficulties with,
or to stop loading something you're not using at the present time:

``` elisp
(use-package ess-site
  :disabled t
  :commands R)
```

When byte-compiling your `.emacs` file, disabled declarations are ommitted
from the output entirely, to accelerate startup times.

## Byte-compiling your .emacs

Another feature of `use-package` is that it always loads every file that it
can when `.emacs` is being byte-compiled.  This helps to silence spurious
warnings about unknown variables and functions.

However, there are times when this is just not enough.  For those times, use
the `:defines` and `:functions` keywords to introduce dummy variable and
function declarations solely for the sake of the byte-compiler:

``` elisp
(use-package texinfo
  :defines texinfo-section-list
  :commands texinfo-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.texi$" . texinfo-mode)))
```

If you need to silence a missing function warning, you can use `:functions`:

``` elisp
(use-package ruby-mode
  :mode "\\.rb\\'"
  :interpreter "ruby"
  :functions inf-ruby-keys
  :config
  (defun my-ruby-mode-hook ()
    (require 'inf-ruby)
    (inf-ruby-keys))

  (add-hook 'ruby-mode-hook 'my-ruby-mode-hook))
```

## Extending the load-path

If your package needs a directory added to the `load-path` in order to load,
use `:load-path`.  This takes a symbol, a function, a string or a list of
strings.  If the path is relative, it is expanded within
`user-emacs-directory`:

``` elisp
(use-package ess-site
  :load-path "site-lisp/ess/lisp/"
  :commands R)
```

Note that when using a symbol or a function to provide a dynamically generated
list of paths, you must inform the byte-compiler of this definition so the
value is available at byte-compilation time.  This is done by using the
special form `eval-and-compile` (as opposed to `eval-when-compile`).  Further,
this value is fixed at whatever was determined during compilation, to avoid
looking up the same information again on each startup:

``` elisp
(eval-and-compile
  (defun ess-site-load-path ()
    (shell-command "find ~ -path ess/lisp")))

(use-package ess-site
  :load-path (lambda () (list (ess-site-load-path)))
  :commands R)
```

## Diminishing minor modes

`use-package` also provides built-in support for the diminish utility -- if
you have that installed.  Its purpose is to remove strings from your mode-line
that provide no useful information.  It is invoked with the `:diminish`
keyword, which is passed either a minor mode symbol, a cons of the symbol and
its replacement string, or just a replacement string, in which case the minor
mode symbol is guessed to be the package name with "-mode" appended at the
end:

``` elisp
(use-package abbrev
  :diminish abbrev-mode
  :config
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file)))
```

## For `package.el` users

You can use `use-package` to load packages from ELPA with `package.el`. This
is particularly useful if you share your `.emacs` among several machines; the
relevant packages are download automatically once declared in your `.emacs`.
The `:ensure` keyword causes the package(s) to be installed automatically if
not already present on your system:

``` elisp
(use-package magit
  :ensure t)
```

If you need to install a different package from the one named by
`use-package`, you can specify it like this:

``` elisp
(use-package tex-site
  :ensure auctex)
```

Lastly, when running on Emacs 24.4 or later, use-package can pin a package to
a specific archive, allowing you to mix and match packages from different
archives.  The primary use-case for this is preferring packages from the
`melpa-stable` and `gnu` archives, but using specific packages from `melpa`
when you need to track newer versions than what is available in the `stable`
archives.

By default `package.el` prefers `melpa` over `melpa-stable` due to the
versioning `(> evil-20141208.623 evil-1.0.9)`, so even if you are tracking
only a single package from `melpa`, you will need to tag all the non-`melpa`
packages with the appropriate archive.

If you want to manually keep a package updated and ignore upstream updates,
you can pin it to `manual`, which as long as there is no repository by that
name, will Just Work(tm).

`use-package` throws an error if you try to pin a package to an archive that
has not been configured using `package-archives` (apart from the magic
`manual` archive mentioned above):

```
Archive 'foo' requested for package 'bar' is not available.
```

Example:

``` elisp
(use-package company
  :ensure t
  :pin "melpa-stable")

(use-package evil
  :ensure t)
  ;; no :pin needed, as package.el will choose the version in melpa

(use-package adaptive-wrap
  :ensure t
  ;; as this package is available only in the gnu archive, this is
  ;; technically not needed, but it helps to highlight where it
  ;; comes from
  :pin "gnu")

(use-package org
  :ensure t
  ;; ignore org-mode from upstream and use a manually installed version
  :pin "manual")
```

**NOTE**: the `:pin` argument has no effect on emacs versions < 24.4.

**NOTE**: if you pin a lot of packages, it will be slightly slower to start
Emacs compared to manually adding all packages to the
`package-pinned-packages` variable.  However, should you do it this way, you
need to keep track of when `(package-initialize)` is called, so letting
`use-package` handle it for you is arguably worth the cost.
