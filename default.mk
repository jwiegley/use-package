TOP := $(dir $(lastword $(MAKEFILE_LIST)))

## User options ######################################################
#
# You can override these settings in "config.mk" or on the command
# line.
#
# You might also want to set LOAD_PATH.  If you do, then it must
# contain "-L .".
#
# If you don't do so, then the default is set in the "Load-Path"
# section below.  The default assumes that all dependencies are
# installed either at "../<DEPENDENCY>", or when using package.el
# at "ELPA_DIR/<DEPENDENCY>-<HIGHEST-VERSION>".

PREFIX   ?= /usr/local
sharedir ?= $(PREFIX)/share
lispdir  ?= $(sharedir)/emacs/site-lisp/use-package
infodir  ?= $(sharedir)/info
docdir   ?= $(sharedir)/doc/use-package
statsdir ?= $(TOP)/stats

CP       ?= install -p -m 644
MKDIR    ?= install -p -m 755 -d
RMDIR    ?= rm -rf
TAR      ?= tar
SED      ?= sed

EMACS    ?= emacs
EMACSBIN ?= $(EMACS)
BATCH     = $(EMACSBIN) -Q --batch $(LOAD_PATH)

INSTALL_INFO     ?= $(shell command -v ginstall-info || printf install-info)
MAKEINFO         ?= makeinfo
MANUAL_HTML_ARGS ?= --css-ref /assets/page.css

## Files #############################################################

PKG      = use-package
PACKAGES = use-package

TEXIPAGES = $(addsuffix .texi,$(filter-out git-commit,$(PACKAGES)))
INFOPAGES = $(addsuffix .info,$(filter-out git-commit,$(PACKAGES)))
HTMLFILES = $(addsuffix .html,$(filter-out git-commit,$(PACKAGES)))
HTMLDIRS  = $(filter-out git-commit,$(PACKAGES))
PDFFILES  = $(addsuffix .pdf,$(filter-out git-commit,$(PACKAGES)))

ELS  = use-package.el
ELS += bind-key.el
ELS += bind-chord.el
ELS += use-package-bind-key.el
ELS += use-package-core.el
ELS += use-package-delight.el
ELS += use-package-diminish.el
ELS += use-package-ensure.el
ELS += use-package-jump.el
ELS += use-package-tests.el
ELS += use-package-chords.el
ELS += use-package-ensure-system-package.el
ELCS = $(ELS:.el=.elc)
ELMS = use-package.el $(filter-out $(addsuffix .el,$(PACKAGES)),$(ELS))
ELGS = 

## Versions ##########################################################

VERSION = 2.4.1

USE_PACKAGE_VERSION = 2.4.1

EMACS_VERSION = 24.3

EMACSOLD := $(shell $(BATCH) --eval \
  "(and (version< emacs-version \"$(EMACS_VERSION)\") (princ \"true\"))")
ifeq "$(EMACSOLD)" "true"
  $(error At least version $(EMACS_VERSION) of Emacs is required)
endif

## Load-Path #########################################################

ifndef LOAD_PATH

ELPA_DIR ?= $(HOME)/.emacs.d/elpa

SYSTYPE := $(shell $(EMACSBIN) -Q --batch --eval "(princ system-type)")
ifeq ($(SYSTYPE), windows-nt)
  CYGPATH := $(shell cygpath --version 2>/dev/null)
endif

LOAD_PATH = -L $(TOP)

endif # ifndef LOAD_PATH

DOC_LOAD_PATH ?= $(LOAD_PATH) \
    -L $(HOME)/emacs/site-lisp \
    -L $(HOME)/emacs/site-lisp/ox-texinfo-plus \
    -L $(HOME)/emacs/site-lisp/org-mode/contrib/lisp
