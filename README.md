# `use-package`

The `use-package` declaration macro allows you to isolate package
configuration in your ".emacs" in a way that is performance-oriented and,
well, just tidy.  I created it because I have over 80 packages that I use
in Emacs, and things were getting difficult to manage.  Yet with this
utility my total load time is just under 1 second, with no loss of
functionality!

Here is the simplest `use-package` declaration:

``` elisp
(use-package foo)
```

This loads in the package foo, but only if foo is available on your system.
If not, a warning is logged to your `*Messages*` buffer.  If it succeeds a
message about "Loading foo" is logged, along with the time it took to load,
if that time is over 0.01s.

Use the :init keyword to do some stuff to initialize foo. If loading
was deferred, the code is run immediately; otherwise the package is
required before running the code.  See below for options that defer
loading of the package.

``` elisp
(use-package foo
  :init
  (progn
    (setq foo-variable t)
    (foo-mode 1)))
```

A very common thing to do when loading a module is to bind a key to primary
commands within that module:

``` elisp
(use-package ace-jump-mode
  :bind ("C-." . ace-jump-mode))
```

This does two things: first, it creates autoload for the `ace-jump-mode`
command, and defers loading of `ace-jump-mode` until you actually use it.
Second, it binds the key `C-.` to that command.  After loading, you can use
`M-x describe-personal-keybindings` to see all such bindings you've set
throughout your Emacs.

A more literal way to do the exact same thing is:

``` elisp
(use-package ace-jump-mode
  :commands ace-jump-mode
  :init
  (bind-key "C-." 'ace-jump-mode))
```

When you use the `:commands` keyword, it creates autoloads for those
commands and defers loading of the module until they are used.  In this
case, the `:init` form is always run -- even if ace-jump-mode might not be
on your system.  So remember to keep `:init` activities to only those that
would succeed either way.

Similar to `:bind`, you can use `:mode` and `:interpreter` to establish a
deferred binding within `auto-mode-alist` and `interpreter-mode-alist`.
The specifier to either keyword can be a single cons, or a list, or just
a string:

``` elisp
(use-package ruby-mode
  :mode "\\.rb\\'"
  :interpreter "ruby")

;; The package is "python" but the mode is "python-mode":
(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode))
```

If you aren't using `:commands`, `:bind`, `:mode`, or `:interpreter` (all
of which imply `:commands`), you can still defer loading with the `:defer`
keyword:

``` elisp
(use-package ace-jump-mode
  :defer t
  :init
  (progn
    (autoload 'ace-jump-mode "ace-jump-mode" nil t)
    (bind-key "C-." 'ace-jump-mode)))
```

This does exactly the same thing as the other two commands above.

You can also override deferring with the `:demand` keyword.

A companion to the `:init` keyword is `:config`.  Although `:init` always
happens in the case of deferred modules (which are likely to be the most
common kind), `:config` form only run after the module has been loaded by
Emacs:

``` elisp
(use-package ace-jump-mode
  :bind ("C-." . ace-jump-mode)
  :config
  (message "Yay, ace-jump-mode was actually loaded!"))
```

You will see a "Configured..." message in your `*Messages*` log when a
package is configured, and a timing if the configuration time was longer
than 0.01s.  You should keep `:init` forms as simple as possible, and put
as much as you can get away with on the `:config` side.

You can have both `:init` and `:config`:

``` elisp
(use-package haskell-mode
  :commands haskell-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.l?hs$" . haskell-mode))
  :config
  (progn
    (use-package inf-haskell)
    (use-package hs-lint)))
```

In this case, I want to autoload the command `haskell-mode` from
"haskell-mode.el", add it to `auto-mode-alist` at the time ".emacs" is
loaded, but wait until after I've opened a Haskell file before loading
"inf-haskell.el" and "hs-lint.el".

Another similar option to `:init` is `:idle`. Like `:init` this always
run, however, it does so when Emacs is idle at some time in the future
(see variable `use-package-idle-interval`) after load. This is
particularly useful for convienience minor modes which can be slow to
load. For instance, in this case, I want Emacs to always use
`global-pabbrev-mode`. `:commands` creates an appropriate autoload;
`:idle` will run this command at some point in the future. If you start
Emacs and begin typing straight away, loading will happen eventually.

``` elisp
(use-package pabbrev
  :commands global-pabbrev-mode
  :idle (global-pabbrev-mode)
  :idle-priority 3)
```

Idle functions are run in the order in which they are evaluated, unless you
specify a priority using `:idle-priority`, in which case lower priority
functions are run first (the default priority is 5). If you have many idle
functions, it may take sometime for all to run. `use-package` will always tell
you if there is an error in the form which can otherwise be difficult to
debug. It may tell you about functions being eval'd, depending on the value of
`use-package-verbose`. Other good candidates for `:idle` are `yasnippet`,
`auto-complete` and `autopair`.

Finally, you may wish to use `:pre-load`. This form runs before everything
else whenever the `use-package` form evals; the package in question will
never have been required. This can be useful, if you wish for instance, to
pull files from a git repository, or mount a file system. Like :init,
keeping this form as simple as possible makes sense.

The `:bind` keyword takes either a cons or a list of conses:

``` elisp
(use-package hi-lock
  :bind (("M-o l" . highlight-lines-matching-regexp)
         ("M-o r" . highlight-regexp)
         ("M-o w" . highlight-phrase)))
```

The `:commands` keyword likewise takes either a symbol or a list of
symbols.

You can use the `:if` keyword to predicate the loading and initialization
of a module.  For example, I only want an `edit-server` running for my
main, graphical Emacs, not for Emacsen I may start at the command line:

``` elisp
(use-package edit-server
  :if window-system
  :init
  (progn
    (add-hook 'after-init-hook 'server-start t)
    (add-hook 'after-init-hook 'edit-server-start t)))
```

The `:disabled` keyword can be used to turn off a module that you're having
difficulties with, or to stop loading something you're not really using at
the present time:

``` elisp
(use-package ess-site
  :disabled t
  :commands R)
```

Another feature of `use-package` is that it always loads every file that it
can when your ".emacs" is being byte-compiled (if you do that, which I
recommend).  This helps to silence spurious warnings about unknown
variables and functions.

However, there are times when this is just not enough.  For those times,
use the `:defines` keyword to introduce empty variable definitions solely
for the sake of the byte-compiler:

``` elisp
(use-package texinfo
  :defines texinfo-section-list
  :commands texinfo-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.texi$" . texinfo-mode)))
```

If you need to silence a missing function warning, do it with an autoload
stub in your `:init` block:

``` elisp
(use-package w3m
  :commands (w3m-browse-url w3m-session-crash-recovery-remove)
  :init
  (eval-when-compile
    (autoload 'w3m-search-escape-query-string "w3m-search")))
```

If your package needs a directory added to the `load-path` in order to load,
use `:load-path`.  It takes a string or a list of strings.  If the path is
relative, it will be expanded within `user-emacs-directory`:

``` elisp
(use-package ess-site
  :disabled t
  :load-path "site-lisp/ess/lisp/"
  :commands R)
```

`use-package` also provides built-in support for the diminish utility,
if you have that installed.  It's purpose is to remove strings from your
mode-line that would otherwise always be there and provide no useful
information.  It is invoked with the `:diminish` keyword, which is passed
either the minor mode symbol, a cons of the symbol and a replacement string,
or just a replacement string in which case the minor mode symbol is guessed
to be the package name with "-mode" at the end:

``` elisp
(use-package abbrev
  :diminish abbrev-mode
  :init
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file))

  :config
  (add-hook 'expand-load-hook
            (lambda ()
              (add-hook 'expand-expand-hook 'indent-according-to-mode)
              (add-hook 'expand-jump-hook 'indent-according-to-mode))))
```

If you noticed that this declaration has neither a `:bind`, `:commands` or
`:defer` keyword: congratulations, you're an A student!  What it means is
that both the `:init` and `:config` forms will be executed when ".emacs" is
loaded, with no delays until later.  Is this useful?  Not really.  I just
happen to like separating my configuration into things that must happen at
startup time, and things that could potentially wait until after the
actual load.  In this case, everything could be put inside `:init` and
there would be no difference.

## For `package.el` users

You can use `use-package` to load packages from ELPA with package.el. This
is particularly useful if you share your .emacs between several machines;
the relevant packages will download automatically once placed in your
.emacs. The `:ensure` key will install the package automatically if it is
not already present:

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

Lastly, when running on Emacs 24.4 or later, use-package can pin a
package to a certain archive, which allows you to mix and match
packages from different archives.

The primary use-case is preferring packages from the melpa-stable and
gnu archives, but with certain packages from melpa when you need to
track newer versions than what is available in the stable archives.

By default package.el will prefer melpa over melpa-stable due to the
versioning (> evil-20141208.623 evil-1.0.9) so even if you are
tracking only a single package from melpa, you will need to tag all
the non-melpa packages with the appropriate archive.

If you want to manually keep a package updated and ignore upstream
updates, you can pin it to "manual" which as long as there is no
repository by that name will Just Work(tm).

`use-package` will throw an error if you try to pin a package to an
archive that has not been configured via `package-archives` (apart
from the magic "manual" archive mentioned above):

```
Archive 'foo' requested for package 'bar' is not available.
```

Example:

``` elisp
(use-package company
  :ensure t
  :pin melpa-stable)

(use-package evil
  :ensure t)
  ;; no :pin needed, as package.el will choose the version in melpa

(use-package adaptive-wrap
  :ensure t
  ;; as this package is available only in the gnu archive, this is
  ;; technically not needed, but it helps to highlight where it
  ;; comes from
  :pin gnu)

(use-package org
  :ensure t
  ;; ignore org-mode from upstream and use a manually installed version
  :pin manual)
```

NOTE: the :pin argument has no effect on emacs versions < 24.4.

NOTE: if you pin a lot of packages, it will be slightly slower to
start emacs compared to manually adding all packages to the
`package-pinned-packages` variable, however should you do it that way,
you will need to keep track of when (package-initialize) is called, so
just let use-package handle it for you.
