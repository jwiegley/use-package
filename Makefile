-include config.mk
include default.mk

## ###################################################################

.PHONY: install install-lisp install-docs install-info \
	test test-interactive use-package \
	clean clean-lisp clean-docs clean-archives \
	stats bump-version melpa-post-release \
	dist use-package-$(VERSION).tar.gz

all: elc docs

help:
	$(info )
	$(info Current version: use-package-$(VERSION))
	$(info )
	$(info See default.mk for variables you might want to set.)
	$(info )
	$(info Build)
	$(info =====)
	$(info )
	$(info make [all]            - compile elisp and documentation)
	$(info make elc              - compile elisp)
	$(info make docs             - generate info manuals)
	$(info make info             - generate info manuals)
	$(info make html             - generate html manual files)
	$(info make html-dir         - generate html manual directories)
	$(info make pdf              - generate pdf manuals)
	$(info )
	$(info Install)
	$(info =======)
	$(info )
	$(info make install          - install elisp and documentation)
	$(info make install-lisp     - install elisp)
	$(info make install-docs     - install all documentation)
	$(info make install-info     - install info manuals only)
	$(info )
	$(info Clean)
	$(info ====)
	$(info )
	$(info make clean            - clean elisp, documentation and tarball)
	$(info make clean-lisp       - clean elisp)
	$(info make clean-docs       - clean docs)
	$(info make clean-archives   - clean release tarball)
	$(info make clean-all        - clean everything except tracked texi)
	$(info make clean-stats      - clean stats)
	$(info )
	$(info Test)
	$(info ====)
	$(info )
	$(info make test             - run tests)
	$(info make test-interactive - run tests interactively)
	$(info make emacs-Q          - run emacs -Q plus Use-Package)
	$(info )
	$(info Release Managment)
	$(info =================)
	$(info )
	$(info make texi             - regenerate texi from org)
	$(info make stats            - regenerate statistics)
	$(info make authors          - regenerate AUTHORS.md)
	$(info make preview-stats    - preview statistics)
	$(info make publish-stats    - publish statistics)
	$(info make preview-manuals  - preview manuals)
	$(info make publish-manuals  - publish manuals)
	$(info make dist             - create tarballs)
	$(info make bump-versions    - bump versions for release)
	$(info make bump-snapshots   - bump versions after release)
	@printf "\n"

## Build #############################################################

elc:
	@$(MAKE) -f Makefile.lisp lisp

docs:
	@$(MAKE) -f Makefile.doc all

info:
	@$(MAKE) -f Makefile.doc info

html:
	@$(MAKE) -f Makefile.doc html

html-dir:
	@$(MAKE) -f Makefile.doc html-dir

pdf:
	@$(MAKE) -f Makefile.doc pdf

## Install ###########################################################

install: install-lisp install-docs

install-lisp:
	@$(MAKE) -f Makefile.lisp install

install-docs: docs
	@$(MAKE) -f Makefile.doc install-docs

install-info: info
	@$(MAKE) -f Makefile.doc install-info

## Test ##############################################################

test:
	@$(BATCH) --eval "(progn\
	(load-file \"use-package-tests.el\")\
	(ert-run-tests-batch-and-exit))"

test-interactive:
	@$(EMACSBIN) -Q $(LOAD_PATH) --eval "(progn\
	(load-file \"use-package-tests.el\")\
	(ert t))"

emacs-Q: clean-lisp
	@$(EMACSBIN) -Q $(LOAD_PATH) --debug-init --eval "(progn\
	(setq debug-on-error t)\
	(require 'use-package))"

## Clean #############################################################

clean: clean-lisp clean-docs clean-archives
	@printf "Cleaning...\n"
	@$(RM) *.elc $(ELGS) # temporary cleanup kludge
	@$(RM) *.texi~ *.info*

clean-lisp:
	@$(MAKE) -f Makefile.lisp clean

clean-docs:
	@$(MAKE) -f Makefile.doc clean

clean-archives:
	@$(RM) *.tar.gz *.tar
	@$(RMDIR) use-package-$(VERSION)

clean-all: clean clean-stats

clean-stats:
	@$(RMDIR) $(statsdir)

## Release management ################################################

texi:
	@$(MAKE) -f Makefile.doc texi

stats:
	@$(MAKE) -f Makefile.doc stats

authors:
	@$(MAKE) -f Makefile.doc authors

preview-stats:
	@$(MAKE) -f Makefile.doc preview-stats

publish-stats:
	@$(MAKE) -f Makefile.doc publish-stats

preview-manuals:
	@$(MAKE) -f Makefile.doc preview-manuals

publish-manuals:
	@$(MAKE) -f Makefile.doc publish-manuals

dist: use-package-$(VERSION).tar.gz

DIST_ROOT_FILES = COPYING default.mk Makefile README.md
DIST_LISP_FILES = $(ELS) Makefile.lisp
DIST_DOCS_FILES = $(TEXIPAGES) AUTHORS.md Makefile.doc

use-package-$(VERSION).tar.gz: lisp info
	@printf "Packing $@\n"
	@$(MKDIR) use-package-$(VERSION)
	@$(CP) $(DIST_ROOT_FILES) use-package-$(VERSION)
	@$(TAR) cz --mtime=./use-package-$(VERSION) -f use-package-$(VERSION).tar.gz use-package-$(VERSION)
	@$(RMDIR) use-package-$(VERSION)

define set_manual_version
(let ((version (split-string "$(USE_PACKAGE_VERSION)" "\\.")))
  (setq version (concat (car version) "." (cadr version)))
  (dolist (file (list "use-package"))
    (with-current-buffer (find-file-noselect (format "%s.org" file))
      (goto-char (point-min))
      (re-search-forward "^#\\+SUBTITLE: for version ")
      (delete-region (point) (line-end-position))
      (insert version)
      (save-buffer))))
endef
export set_manual_version

bump-versions: bump-versions-1 texi
bump-versions-1:
	@$(BATCH) --eval "(progn\
        $$set_manual_version)"

bump-snapshots:
	@$(BATCH) --eval "(progn\
        $$set_package_requires)"
	git commit -a -m "Reset Package-Requires for Melpa"
