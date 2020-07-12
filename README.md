# `use-package`

[![Join the chat at https://gitter.im/use-package/Lobby](https://badges.gitter.im/use-package/Lobby.svg)](https://gitter.im/use-package/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![Build Status](https://travis-ci.org/jwiegley/use-package.svg?branch=master)](https://travis-ci.org/jwiegley/use-package)
[![MELPA](http://melpa.milkbox.net/packages/use-package-badge.svg)](http://melpa.milkbox.net/#/use-package)
[![MELPA Stable](https://stable.melpa.org/packages/use-package-badge.svg)](https://stable.melpa.org/#/use-package)

The `use-package` macro allows you to isolate package configuration in your
`.emacs` file in a way that is both performance-oriented and, well, tidy.  I
created it because I have over 80 packages that I use in Emacs, and things
were getting difficult to manage.  Yet with this utility my total load time is
around 2 seconds, with no loss of functionality!

**NOTE**: `use-package` is **not** a package manager! Although `use-package`
does have the useful capability to interface with package managers (see
[below](#package-installation)), its primary purpose is for the configuration
and loading of packages.

Notes for users upgrading to 2.x are located [at the bottom](#upgrading-to-2x).

- [Installing use-package](#installing-use-package)
- [Getting started](#getting-started)
- [Key-binding](#key-binding)
	+ [Binding to keymaps](#binding-to-keymaps)
	+ [Binding within local keymaps](#binding-within-local-keymaps)
- [Modes and interpreters](#modes-and-interpreters)
- [Magic handlers](#magic-handlers)
- [Hooks](#hooks)
- [Package customization](#package-customization)
  + [Customizing variables](#customizing-variables)
  + [Customizing faces](#customizing-faces)
- [Notes about lazy loading](#notes-about-lazy-loading)
- [Information about package loads](#information-about-package-loads)
- [Conditional loading](#conditional-loading)
	+ [Conditional loading before :preface](#conditional-loading-before-preface)
	+ [Loading packages in a sequence](#loading-packages-in-sequence)
	+ [Prevent loading if dependencies are missing](#prevent-loading-if-dependencies-are-missing)
- [Byte compiling your .emacs](#byte-compiling-your-emacs)
	+ [Prevent a package from loading at compile-time](#prevent-a-package-from-loading-at-compile-time)
- [Extending the load-path](#extending-the-load-path)
- [Catching errors during use-package expansion](#catching-errors-during-use-package-expansion)
- [Diminishing and delighting minor modes](#diminishing-and-delighting-minor-modes)
- [Package installation](#package-installation)
	+ [Usage with other package managers](#usage-with-other-package-managers)
- [Gathering Statistics](#gathering-statistics)
- [Keyword Extensions](#keyword-extensions)
	+ [use-package-ensure-system-package](#use-package-ensure-system-package)
	+ [use-package-chords](#use-package-chords)
	+ [How to create an extension](#how-to-create-an-extension)
		+ [First step: Add the keyword](#first-step-add-the-keyword)
		+ [Second step: Create a normalizer](#second-step-create-a-normalizer)
		+ [Third step: Create a handler](#third-step-create-a-handler)
		+ [Fourth step: Test it out](#fourth-step-test-it-out)
- [Some timing results](#some-timing-results)
* [Upgrading to 2.x](#upgrading-to-2x)
	+ [Semantics of :init is now consistent](#semantics-of-init-is-now-consistent)
	+ [:idle has been removed](#idle-has-been-removed)
	+ [:defer now accepts an optional numeric argument](#defer-now-accepts-an-optional-numeric-argument)
	+ [Add :preface, occuring before everything except :disabled](#add-preface-occurring-before-everything-except-disabled)
	+ [Add :functions, for declaring functions to the byte-compiler](#add-functions-for-declaring-functions-to-the-byte-compiler)
	+ [use-package.el is no longer needed at runtime](#use-packageel-is-no-longer-needed-at-runtime)
## Installing use-package

Either clone from this GitHub repository or install from
[MELPA](http://melpa.milkbox.net/) (recommended).

## Getting started

Here is the simplest `use-package` declaration:

``` elisp
;; This is only needed once, near the top of the file
(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (add-to-list 'load-path "<path where use-package is installed>")
  (require 'use-package))

(use-package foo)
```

This loads in the package `foo`, but only if `foo` is available on your
system. If not, a warning is logged to the `*Messages*` buffer.

Use the `:init` keyword to execute code before a package is loaded.  It
accepts one or more forms, up to the next keyword:

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

**NOTE**: inside strings, special keys like `tab` or `F1`-`Fn` have to be written inside angle brackets, e.g. `"C-<up>"`.
Standalone special keys (and some combinations) can be written in square brackets, e.g. `[tab]` instead of `"<tab>"`. The syntax for the keybindings is similar to
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

Furthermore, [remapping commands](https://www.gnu.org/software/emacs/manual/html_node/elisp/Remapping-Commands.html)
with `:bind` and `bind-key` works as expected, because when the
binding is a vector, it is passed straight to `define-key`. So the
following example will rebind `M-q` (originally `fill-paragraph`) to
`unfill-toggle`:

``` elisp
(use-package unfill
  :bind ([remap fill-paragraph] . unfill-toggle))
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

For example:

``` elisp
(use-package projectile
  :bind-keymap
  ("C-c p" . projectile-command-map))
```

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
variables. The specifier to either keyword can be a cons cell, a list of cons
cells, or a string or regexp:

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
`:bind-keymap*`, `:mode`, `:interpreter`, or `:hook` (all of which imply `:defer`; see
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

## Magic handlers

Similar to `:mode` and `:interpreter`, you can also use `:magic` and
`:magic-fallback` to cause certain function to be run if the beginning of a
file matches a given regular expression. The difference between the two is
that `:magic-fallback` has a lower priority than `:mode`. For example:

``` elisp
(use-package pdf-tools
  :load-path "site-lisp/pdf-tools/lisp"
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install :no-query))
```

This registers an autoloaded command for `pdf-view-mode`, defers loading of
`pdf-tools`, and runs `pdf-view-mode` if the beginning of a buffer matches the
string `"%PDF"`.

## Hooks

The `:hook` keyword allows adding functions onto package hooks. Thus,
all of the following are equivalent:

``` elisp
(use-package ace-jump-mode
  :hook prog-mode)

(use-package ace-jump-mode
  :hook (prog-mode . ace-jump-mode))

(use-package ace-jump-mode
  :commands ace-jump-mode
  :init
  (add-hook 'prog-mode-hook #'ace-jump-mode))
```

And likewise, when multiple hooks should be applied, the following are also
equivalent:

``` elisp
(use-package ace-jump-mode
  :hook (prog-mode text-mode))

(use-package ace-jump-mode
  :hook ((prog-mode text-mode) . ace-jump-mode))

(use-package ace-jump-mode
  :hook ((prog-mode . ace-jump-mode)
         (text-mode . ace-jump-mode)))

(use-package ace-jump-mode
  :commands ace-jump-mode
  :init
  (add-hook 'prog-mode-hook #'ace-jump-mode)
  (add-hook 'text-mode-hook #'ace-jump-mode))
```

When using `:hook` omit the "-hook" suffix if you specify the hook
explicitly, as this is appended by default. For example the following
code will not work as it attempts to add to the `prog-mode-hook-hook`
which does not exist:

``` elisp
;; DOES NOT WORK
(use-package ace-jump-mode
  :hook (prog-mode-hook . ace-jump-mode))
```

If you do not like this behaviour, set `use-package-hook-name-suffix`
to nil. By default the value of this variable is "-hook".

The use of `:hook`, as with `:bind`, `:mode`, `:interpreter`, etc., causes the
functions being hooked to implicitly be read as `:commands` (meaning they will
establish interactive `autoload` definitions for that module, if not already
defined as functions), and so `:defer t` is also implied by `:hook`.

## Package customization

### Customizing variables.

The `:custom` keyword allows customization of package custom variables.

``` elisp
(use-package comint
  :custom
  (comint-buffer-maximum-size 20000 "Increase comint buffer size.")
  (comint-prompt-read-only t "Make the prompt read only."))
```

The documentation string is not mandatory.

**NOTE**: these are only for people who wish to keep customizations with their
accompanying use-package declarations. Functionally, the only benefit over
using `setq` in a `:config` block is that customizations might execute code
when values are assigned.

**NOTE**: The customized values are **not** saved in the Emacs `custom-file`.
Thus you should either use the `:custom` option **or** you should use `M-x
customize-option` which will save customized values in the Emacs `custom-file`.
Do not use both.

### Customizing faces

The `:custom-face` keyword allows customization of package custom faces.

``` elisp
(use-package eruby-mode
  :custom-face
  (eruby-standard-face ((t (:slant italic)))))
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

``` elisp
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

When byte-compiling your `.emacs` file, disabled declarations are omitted
from the output entirely, to accelerate startup times.

**NOTE**: `:when` is provided as an alias for `:if`, and `:unless foo` means
the same thing as `:if (not foo)`. For example, the following will also stop
`:ensure` from happening on Mac systems:

``` elisp
(when (memq window-system '(mac ns))
  (use-package exec-path-from-shell
    :ensure t
    :config
    (exec-path-from-shell-initialize)))
```

### Conditional loading before :preface

If you need to conditionalize a use-package form so that the condition occurs
before even the `:preface` is executed, simply use `when` around the
use-package form itself:

### Loading packages in sequence

Sometimes it only makes sense to configure a package after another has been
loaded, because certain variables or functions are not in scope until that
time. This can achieved using an `:after` keyword that allows a fairly rich
description of the exact conditions when loading should occur. Here is an
example:

``` elisp
(use-package hydra
  :load-path "site-lisp/hydra")

(use-package ivy
  :load-path "site-lisp/swiper")

(use-package ivy-hydra
  :after (ivy hydra))
```

In this case, because all of these packages are demand-loaded in the order
they occur, the use of `:after` is not strictly necessary. By using it,
however, the above code becomes order-independent, without an implicit
depedence on the nature of your init file.

By default, `:after (foo bar)` is the same as `:after (:all foo bar)`, meaning
that loading of the given package will not happen until both `foo` and `bar`
have been loaded. Here are some of the other possibilities:

``` elisp
:after (foo bar)
:after (:all foo bar)
:after (:any foo bar)
:after (:all (:any foo bar) (:any baz quux))
:after (:any (:all foo bar) (:all baz quux))
```

When you nest selectors, such as `(:any (:all foo bar) (:all baz quux))`, it
means that the package will be loaded when either both `foo` and `bar` have
been loaded, or both `baz` and `quux` have been loaded.

**NOTE**: pay attention if you set `use-package-always-defer` to t, and also use
the `:after` keyword, as you will need to specify how the declared package is
to be loaded: e.g., by some `:bind`. If you're not using one of the mechanisms
that registers autoloads, such as `:bind` or `:hook`, and your package manager
does not provide autoloads, it's possible that without adding `:demand t` to
those declarations, your package will never be loaded.

### Prevent loading if dependencies are missing

While the `:after` keyword delays loading until the dependencies are loaded,
the somewhat simpler `:requires` keyword simply never loads the package if the
dependencies are not available at the time the `use-package` declaration is
encountered. By "available" in this context it means that `foo` is available
if `(featurep 'foo)` evaluates to a non-nil value. For example:

``` elisp
(use-package abbrev
  :requires foo)
```

This is the same as:

``` elisp
(use-package abbrev
  :if (featurep 'foo))
```

As a convenience, a list of such packages may be specified:

``` elisp
(use-package abbrev
  :requires (foo bar baz))
```

For more complex logic, such as that supported by `:after`, simply use `:if`
and the appropriate Lisp expression.

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
such cases, use the `:no-require` keyword:

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

**NOTE**: when using a symbol or a function to provide a dynamically generated
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

## Catching errors during use-package expansion

By default, if `use-package-expand-minimally` is nil (the default),
use-package will attempts to catch and report errors that occur during
expansion of use-package declarations in your init file. Setting
`use-package-expand-minimally` to t completely disables this checking.

This behavior may be overridden locally using the `:catch` keyword. If `t` or
`nil`, it enables or disables catching errors at load time. It can also be a
function taking two arguments: the keyword being processed at the time the
error was encountered, and the error object (as generated by
`condition-case`). For example:

``` elisp
(use-package example
  ;; Note that errors are never trapped in the preface, since doing so would
  ;; hide definitions from the byte-compiler.
  :preface (message "I'm here at byte-compile and load time.")
  :init (message "I'm always here at startup")
  :config
  (message "I'm always here after the package is loaded")
  (error "oops")
  ;; Don't try to (require 'example), this is just an example!
  :no-require t
  :catch (lambda (keyword err)
           (message (error-message-string err))))
```

Evaluating the above form will print these messages:

```
I’m here at byte-compile and load time.
I’m always here at startup
Configuring package example...
I’m always here after the package is loaded
oops
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
not already present on your system:

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

Enable `use-package-always-ensure` if you wish this behavior to be global
for all packages:

``` elisp
(require 'use-package-ensure)
(setq use-package-always-ensure t)
```

**NOTE**: `:ensure` will install a package if it is not already installed, but
it does not keep it up-to-date. If you want to keep your packages updated
automatically, one option is to use
[auto-package-update](https://github.com/rranelli/auto-package-update.el),
like

``` elisp
(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))
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

## Gathering Statistics

If you'd like to see how many packages you've loaded, what stage of
initialization they've reached, and how much aggregate time they've
spent (roughly), you can enable `use-package-compute-statistics` after
loading `use-package` but before any `use-package` forms, and then run
the command `M-x use-package-report` to see the results. The buffer
displayed is a tabulated list. You can use `S` in a column to sort the
rows based on it.

## Keyword Extensions

Starting with version 2.0, `use-package` is based on an extensible
framework that makes it easy for package authors to add new keywords,
or modify the behavior of existing keywords.

Some keyword extensions are now included in the `use-package`
distribution and can be optionally installed.

### `(use-package-ensure-system-package)`

The `:ensure-system-package` keyword allows you to ensure system
binaries exist alongside your package declarations.

First, you will want to make sure `exec-path` is cognisant of all
binary package names that you would like to ensure are
installed. [`exec-path-from-shell`](https://github.com/purcell/exec-path-from-shell)
is often a good way to do this.

To enable the extension after you've loaded `use-package`:

``` elisp
(use-package use-package-ensure-system-package
  :ensure t)
```

Here’s an example of usage:

``` emacs-lisp
(use-package rg
  :ensure-system-package rg)
```

This will expect a global binary package to exist called `rg`. If it
does not, it will use your system package manager (using the package
[`system-packages`](https://gitlab.com/jabranham/system-packages)) to
attempt an install of a binary by the same name asynchronously. For
example, for most `macOS` users this would call: `brew install rg`.

If the package is named differently than the binary, you can use a
cons in the form of  `(binary . package-name)`, i.e.:

``` emacs-lisp
(use-package rg
  :ensure-system-package
  (rg . ripgrep))
```

In the previous `macOS` example, this would call: `brew install
ripgrep` if `rg` was not found.

What if you want to customize the install command further?

``` emacs-lisp
(use-package tern
  :ensure-system-package (tern . "npm i -g tern"))
```

`:ensure-system-package` can also take a cons where its `cdr` is a
string that will get called by `(async-shell-command)` to install if
it isn’t found.

You may also pass in a list of cons-es:

``` emacs-lisp
(use-package ruby-mode
  :ensure-system-package
  ((rubocop     . "gem install rubocop")
   (ruby-lint   . "gem install ruby-lint")
   (ripper-tags . "gem install ripper-tags")
   (pry         . "gem install pry")))
```

Finally, in case the package dependency does not provide a global
executable, you can ensure packages exist by checking the presence of
a file path by providing a string like so:

``` emacs-lisp
(use-package dash-at-point
  :if (eq system-type 'darwin)
  :ensure-system-package
  ("/Applications/Dash.app" . "brew cask install dash"))
```

`:ensure-system-package` will use `system-packages-install` to install
system packages, except where a custom command has been specified, in
which case it will be executed verbatim by `async-shell-command`.

Configuration variables `system-packages-package-manager` and
`system-packages-use-sudo` will be honoured, but not for custom
commands. Custom commands should include the call to sudo in the
command if needed.

### `(use-package-chords)`

The `:chords` keyword allows you to define
[`key-chord`](http://www.emacswiki.org/emacs/key-chord.el) bindings
for `use-package` declarations in the same manner as the `:bind`
keyword.

To enable the extension:

``` elisp
(use-package use-package-chords
  :ensure t
  :config (key-chord-mode 1))
```

Then you can define your chord bindings in the same manner as `:bind` using a cons or a list of conses:

``` elisp
(use-package ace-jump-mode
  :chords (("jj" . ace-jump-char-mode)
           ("jk" . ace-jump-word-mode)
           ("jl" . ace-jump-line-mode)))
```

### How to create an extension

#### First step: Add the keyword

The first step is to add your keyword at the right place in
`use-package-keywords`.  This list determines the order in which things will
happen in the expanded code.  You should never change this order, but it gives
you a framework within which to decide when your keyword should fire.

#### Second step: Create a normalizer

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

#### Third step: Create a handler

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

#### Fourth step: Test it out

After the keyword has been inserted into `use-package-keywords`, and a
normalizer and a handler defined, you can now test it by seeing how usages of
the keyword will expand.  For this, use `M-x pp-macroexpand-last-sexp` with
the cursor set immediately after the `(use-package ...)` expression.

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

## :defer now accepts an optional numeric argument

`:defer [N]` causes the package to be loaded -- if it has not already been --
after `N` seconds of idle time.

``` elisp
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

**NOTE**: whatever is specified within `:preface` is evaluated both at load
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
