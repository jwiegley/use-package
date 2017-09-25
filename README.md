# `use-package`

[![Build Status](https://travis-ci.org/jwiegley/use-package.svg?branch=master)](https://travis-ci.org/jwiegley/use-package)

The `use-package` macro allows you to isolate package configuration in your
`.emacs` file in a way that is both performance-oriented and, well, tidy.  I
created it because I have over 80 packages that I use in Emacs, and things
were getting difficult to manage.  Yet with this utility my total load time is
around 2 seconds, with no loss of functionality!

Notes for users upgrading to 2.x are located [at the bottom](#upgrading-to-2x).

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
accepts one or more forms, up until the next keyword:

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
  :bind (("M-s O" . moccur)
         :map isearch-mode-map
         ("M-o" . isearch-moccur)
         ("M-O" . isearch-moccur-all))
  :init
  (setq isearch-lazy-highlight t)
  :config
  (use-package moccur-edit))
```

In this case, I want to autoload the commands `isearch-moccur` and
`isearch-all` from `color-moccur.el`, and bind keys both at the global level
and within the `isearch-mode-map` (see next section).  When the package is
actually loaded (by using one of these commands), `moccur-edit` is also
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

NOTE: Special keys like `tab` or `F1`-`Fn` can be written in square brackets,
i.e. `[tab]` instead of `"tab"`. The syntax for the keybindings is similar to
the "kbd" syntax: see [https://www.gnu.org/software/emacs/manual/html_node/emacs/Init-Rebinding.html](https://www.gnu.org/software/emacs/manual/html_node/emacs/Init-Rebinding.html)
for more information.

Examples:

``` elisp
(use-package helm
  :bind (("M-x" . helm-M-x)
         ("M-<f5>" . helm-find-files)
         ([f10] . helm-buffers-list)
         ([S-f10] . helm-recentf)))
```


### Binding to keymaps

Normally `:bind` expects that commands are functions that will be autoloaded
from the given package. However, this does not work if one of those commands
is actually a keymap, since keymaps are not functions, and cannot be
autoloaded using Emacs' `autoload` mechanism.

To handle this case, `use-package` offers a special, limited variant of
`:bind` called `:bind-keymap`. The only difference is that the "commands"
bound to by `:bind-keymap` must be keymaps defined in the package, rather than
command functions. This is handled behind the scenes by generating custom code
that loads the package containing the keymap, and then re-executes your
keypress after the first load, to reinterpret that keypress as a prefix key.

### Binding within local keymaps

Slightly different from binding a key to a keymap, is binding a key *within* a
local keymap that only exists after the package is loaded.  `use-package`
supports this with a `:map` modifier, taking the local keymap to bind to:

``` elisp
(use-package helm
  :bind (:map helm-command-map
         ("C-c h" . helm-execute-persistent-action)))
```

The effect of this statement is to wait until `helm` has loaded, and then to
bind the key `C-c h` to `helm-execute-persistent-action` within Helm's local
keymap, `helm-mode-map`.

Multiple uses of `:map` may be specified. Any binding occurring before the
first use of `:map` are applied to the global keymap:

``` elisp
(use-package term
  :bind (("C-c t" . term)
         :map term-mode-map
         ("M-p" . term-send-up)
         ("M-n" . term-send-down)
         :map term-raw-map
         ("M-o" . other-window)
         ("M-p" . term-send-up)
         ("M-n" . term-send-down)))
```

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
`:bind-keymap*`, `:mode`, or `:interpreter` (all of which imply `:defer`; see
the docstring for `use-package` for a brief description of each), you can
still defer loading with the `:defer` keyword:

``` elisp
(use-package ace-jump-mode
  :defer t
  :init
  (autoload 'ace-jump-mode "ace-jump-mode" nil t)
  (bind-key "C-." 'ace-jump-mode))
```

This does exactly the same thing as the following:

``` elisp
(use-package ace-jump-mode
  :bind ("C-." . ace-jump-mode))
```

## Notes about lazy loading

In almost all cases you don't need to manually specify `:defer t`.  This is
implied whenever `:bind` or `:mode` or `:interpreter` is used.  Typically, you
only need to specify `:defer` if you know for a fact that some other package
will do something to cause your package to load at the appropriate time, and
thus you would like to defer loading even though use-package isn't creating
any autoloads for you.

You can override package deferral with the `:demand` keyword.  Thus, even if
you use `:bind`, using `:demand` will force loading to occur immediately and
not establish an autoload for the bound key.

## Information about package loads

When a package is loaded, and if you have `use-package-verbose` set to `t`, or
if the package takes longer than 0.1s to load, you will see a message to
indicate this loading activity in the `*Messages*` buffer.  The same will
happen for configuration, or `:config` blocks that take longer than 0.1s to
execute.  In general, you should keep `:init` forms as simple and quick as
possible, and put as much as you can get away with into the `:config` block.
This way, deferred loading can help your Emacs to start as quickly as
possible.

Additionally, if an error occurs while initializing or configuring a package,
this will not stop your Emacs from loading.  Rather, the error will be
captured by `use-package`, and reported to a special `*Warnings*` popup
buffer, so that you can debug the situation in an otherwise functional Emacs.

## Conditional loading

You can use the `:if` keyword to predicate the loading and initialization of
modules.

For example, I only want `edit-server` running for my main,
graphical Emacs, not for other Emacsen I may start at the command line:

``` elisp
(use-package edit-server
  :if window-system
  :init
  (add-hook 'after-init-hook 'server-start t)
  (add-hook 'after-init-hook 'edit-server-start t))
```
In another example, we can load things conditional on the operating system:

```
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :config
  (exec-path-from-shell-initialize))
```

The `:disabled` keyword can turn off a module you're having difficulties with,
or stop loading something you're not using at the present time:

``` elisp
(use-package ess-site
  :disabled
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

### Prevent a package from loading at compile-time

Normally, `use-package` will load each package at compile time before
compiling the configuration, to ensure that any necessary symbols are in scope
to satisfy the byte-compiler.  At times this can cause problems, since a
package may have special loading requirements, and all that you want to use
`use-package` for is to add a configuration to the `eval-after-load` hook.  In
such cases, use the `:no-require` keyword, which implies `:defer`:

``` elisp
(use-package foo
  :no-require t
  :config
  (message "This is evaluated when `foo' is loaded"))
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

## Diminishing and delighting minor modes

`use-package` also provides built-in support for the diminish and
delight utilities -- if you have them installed. Their purpose is to
remove or change minor mode strings in your mode-line.

[diminish](https://github.com/myrjola/diminish.el) is invoked with
the `:diminish` keyword, which is passed either a minor mode symbol, a
cons of the symbol and its replacement string, or just a replacement
string, in which case the minor mode symbol is guessed to be the
package name with "-mode" appended at the end:

``` elisp
(use-package abbrev
  :diminish abbrev-mode
  :config
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file)))
```

[delight](https://elpa.gnu.org/packages/delight.html) is invoked with
the `:delight` keyword, which is passed a minor mode symbol, a
replacement string or
quoted
[mode-line data](https://www.gnu.org/software/emacs/manual/html_node/elisp/Mode-Line-Data.html) (in
which case the minor mode symbol is guessed to be the package name
with "-mode" appended at the end), both of these, or several lists of
both. If no arguments are provided, the default mode name is hidden
completely.

``` elisp
;; Don't show anything for rainbow-mode.
(use-package rainbow-mode
  :delight)

;; Don't show anything for auto-revert-mode, which doesn't match
;; its package name.
(use-package autorevert
  :delight auto-revert-mode)

;; Remove the mode name for projectile-mode, but show the project name.
(use-package projectile
  :delight '(:eval (concat " " (projectile-project-name))))

;; Completely hide visual-line-mode and change auto-fill-mode to " AF".
(use-package emacs
  :delight
  (auto-fill-function " AF")
  (visual-line-mode))
```

## Package installation

You can use `use-package` to load packages from ELPA with `package.el`. This
is particularly useful if you share your `.emacs` among several machines; the
relevant packages are downloaded automatically once declared in your `.emacs`.
The `:ensure` keyword causes the package(s) to be installed automatically if
not already present on your system (set `(setq use-package-always-ensure t)`
if you wish this behavior to be global for all packages):

``` elisp
(use-package magit
  :ensure t)
```

If you need to install a different package from the one named by
`use-package`, you can specify it like this:

``` elisp
(use-package tex
  :ensure auctex)
```

Lastly, when running on Emacs 24.4 or later, use-package can pin a package to
a specific archive, allowing you to mix and match packages from different
archives.  The primary use-case for this is preferring packages from the
`melpa-stable` and `gnu` archives, but using specific packages from `melpa`
when you need to track newer versions than what is available in the `stable`
archives is also a valid use-case.

By default `package.el` prefers `melpa` over `melpa-stable` due to the
versioning `(> evil-20141208.623 evil-1.0.9)`, so even if you are tracking
only a single package from `melpa`, you will need to tag all the non-`melpa`
packages with the appropriate archive. If this really annoys you, then you can
set `use-package-always-pin` to set a default.

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

**NOTE**: the `:pin` argument has no effect on emacs versions < 24.4.

### Usage with other package managers

By overriding `use-package-ensure-function` and/or
`use-package-pre-ensure-function`, other package managers can override
`:ensure` to use them instead of `package.el`. At the present time,
the only package manager that does this
is [`straight.el`](https://github.com/raxod502/straight.el).

### Deferred installation

`use-package` can defer the installation of a package until it is
first used. To trigger this behavior, specify `:defer-install t` in
the `use-package` form. (This will only have an effect with `:defer t`
and `:ensure t`, of course.)

The package will then be installed when an autoload that was generated
by `use-package` was triggered, or if the feature is loaded by an
`:after` clause. However, it is important to understand the
limitations of this mechanism: deferred installation will *not* be
triggered when you `require` the feature, nor when you call a function
that is autoloaded by the package but not by `use-package`. Thus, if
you specify `:defer-install t`, you will also need to make sure that
any autoloads which are reasonable entry points are specifically
generated by `use-package` (via `:commands`, `:mode`, etc.).

In your code, or interactively, you can trigger the installation of a
package whose installation was deferred using
`use-package-install-deferred-package`.

Deferred installation is *not* currently compatible with
byte-compilation.

## Extending use-package with new or modified keywords

Starting with version 2.0, `use-package` is based on an extensible framework
that makes it easy for package authors to add new keywords, or modify the
behavior of existing keywords.

### First step: Add the keyword

The first step is to add your keyword at the right place in
`use-package-keywords`.  This list determines the order in which things will
happen in the expanded code.  You should never change this order, but it gives
you a framework within which to decide when your keyword should fire.

### Second step: Create a normalizer

Define a normalizer for your keyword by defining a function named after the
keyword, for example:

``` elisp
(defun use-package-normalize/:pin (name-symbol keyword args)
  (use-package-only-one (symbol-name keyword) args
    (lambda (label arg)
      (cond
       ((stringp arg) arg)
       ((symbolp arg) (symbol-name arg))
       (t
        (use-package-error
         ":pin wants an archive name (a string)"))))))
```

The job of the normalizer is take a list of arguments (possibly nil), and turn
it into the single argument (which could still be a list) that should appear
in the final property list used by `use-package`.

### Third step: Create a handler

Once you have a normalizer, you must create a handler for the keyword:

``` elisp
(defun use-package-handler/:pin (name-symbol keyword archive-name rest state)
  (let ((body (use-package-process-keywords name-symbol rest state)))
    ;; This happens at macro expansion time, not when the expanded code is
    ;; compiled or evaluated.
    (if (null archive-name)
        body
      (use-package-pin-package name-symbol archive-name)
      (use-package-concat
       body
       `((push '(,name-symbol . ,archive-name)
               package-pinned-packages))))))
```

Handlers can affect the handling of keywords in two ways.  First, it can
modify the `state` plist before recursively processing the remaining keywords,
to influence keywords that pay attention to the state (one example is the
state keyword `:deferred`, not to be confused with the `use-package` keyword
`:defer`).  Then, once the remaining keywords have been handled and their
resulting forms returned, the handler may manipulate, extend, or just ignore
those forms.

The task of each handler is to return a *list of forms* representing code to
be inserted.  It does not need to be a `progn` list, as this is handled
automatically in other places.  Thus it is very common to see the idiom of
using `use-package-concat` to add new functionality before or after a code
body, so that only the minimum code necessary is emitted as the result of a
`use-package` expansion.

### Fourth step: Test it out

After the keyword has been inserted into `use-package-keywords`, and a
normalizer and a handler defined, you can now test it by seeing how usages of
the keyword will expand.  For this, temporarily set `use-package-debug` to
`t`, and just evaluate the `use-package` declaration.  The expansion will be
shown in a special buffer called `*use-package*`.

## Some timing results

On my Retina iMac, the "Mac port" variant of Emacs 24.4 loads in 0.57s, with
around 218 packages configured (nearly all of them lazy-loaded).  However, I
experience no loss of functionality, just a bit of latency when I'm first
starting to use Emacs (due to the autoloading).  Since I also use idle-loading
for many packages, perceived latency is typically reduced overall.

On Linux, the same configuration loads in 0.32s.

If I don't use Emacs graphically, I can test the absolute minimum times.  This
is done by running:

``` bash
time emacs -l init.elc -batch --eval '(message "Hello, world!")'
```

On the Mac I see an average of 0.36s for the same configuration, and on Linux
0.26s.

# Upgrading to 2.x

## Semantics of :init is now consistent

The meaning of `:init` has been changed: It now *always* happens before
package load, whether `:config` has been deferred or not.  This means that
some uses of `:init` in your configuration may need to be changed to `:config`
(in the non-deferred case).  For the deferred case, the behavior is unchanged
from before.

Also, because `:init` and `:config` now mean "before" and "after", the `:pre-`
and `:post-` keywords are gone, as they should no longer be necessary.

Lastly, an effort has been made to make your Emacs start even in the presence
of use-package configuration failures.  So after this change, be sure to check
your `*Messages*` buffer.  Most likely, you will have several instances where
you are using `:init`, but should be using `:config` (this was the case for me
in a number of places).

## :idle has been removed

I am removing this feature for now because it can result in a nasty
inconsistency.  Consider the following definition:

``` elisp
(use-package vkill
  :commands vkill
  :idle (some-important-configuration-here)
  :bind ("C-x L" . vkill-and-helm-occur)
  :init
  (defun vkill-and-helm-occur ()
    (interactive)
    (vkill)
    (call-interactively #'helm-occur))

  :config
  (setq vkill-show-all-processes t))
```

If I load my Emacs and wait until the idle timer fires, then this is the
sequence of events:

    :init :idle <load> :config

But if I load Emacs and immediately type C-x L without waiting for the idle
timer to fire, this is the sequence of events:

    :init <load> :config :idle

It's possible that the user could use `featurep` in their idle to test for
this case, but that's a subtlety I'd rather avoid.

## :defer now accepts an optional integer argument

`:defer [N]` causes the package to be loaded -- if it has not already been --
after `N` seconds of idle time.

```
(use-package back-button
  :commands (back-button-mode)
  :defer 2
  :init
  (setq back-button-show-toolbar-buttons nil)
  :config
  (back-button-mode 1))
```

## Add :preface, occurring before everything except :disabled

`:preface` can be used to establish function and variable definitions that
will 1) make the byte-compiler happy (it won't complain about functions whose
definitions are unknown because you have them within a guard block), and 2)
allow you to define code that can be used in an `:if` test.

Note that whatever is specified within `:preface` is evaluated both at load
time and at byte-compilation time, in order to ensure that definitions are
seen by both the Lisp evaluator and the byte-compiler, so you should avoid
having any side-effects in your preface, and restrict it merely to symbol
declarations and definitions.

## Add :functions, for declaring functions to the byte-compiler

What `:defines` does for variables, `:functions` does for functions.

## use-package.el is no longer needed at runtime

This means you should put the following at the top of your Emacs, to further
reduce load time:

``` elisp
(eval-when-compile
  (require 'use-package))
(require 'diminish)                ;; if you use :diminish
(require 'bind-key)                ;; if you use any :bind variant
```
