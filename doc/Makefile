# Time-stamp: <2018-01-27 22:11:28 kmodi>

# Makefile to generate use-package doc site

EMACS ?= emacs
EMACS_exists := $(shell command -v $(EMACS) 2> /dev/null)
ifeq ("$(EMACS_exists)","")
	EMACS := /tmp/emacs/bin/emacs
endif

# EMACS_BIN_SOURCE and EMACS_VERSION are used later in the vcheck rule
# only if EMACS_exists has evaluated to "".
EMACS_BIN_SOURCE ?= https://github.com/npostavs/emacs-travis/releases/download/bins
EMACS_VERSION ?= 25.3

# Directory where the required elisp packages are auto-installed
TMPDIR ?= /tmp
OX_HUGO_ELPA=$(TMPDIR)/$(USER)/ox-hugo-dev/

HUGO ?= hugo
HUGO_exists := $(shell command -v $(HUGO) 2> /dev/null)
ifeq ("$(HUGO_exists)","")
	HUGO := $(OX_HUGO_ELPA)hugo
endif

# HUGO_VERSION and HUGO_OS are used later in the vcheck rule only if
# HUGO_exists has evaluated to "".
HUGO_VERSION ?= 0.34
# |--------------------|
# | HUGO_OS            |
# |--------------------|
# | DragonFlyBSD-64bit |
# | FreeBSD-32bit      |
# | FreeBSD-64bit      |
# | FreeBSD-ARM        |
# | Linux-32bit        |
# | Linux-64bit        |
# | Linux-ARM          |
# | Linux-ARM64        |
# | NetBSD-32bit       |
# | NetBSD-64bit       |
# | NetBSD-ARM         |
# | OpenBSD-32bit      |
# | OpenBSD-64bit      |
# | macOS-32bit        |
# | macOS-64bit        |
# |--------------------|
HUGO_OS ?= Linux-64bit

# Directory containing the Hugo site's config.toml
HUGO_BASE_DIR=./
# Value to be passed to hugo's --baseURL argument
HUGO_BASE_URL ?= http://localhost
# Other hugo arguments
HUGO_ARGS=

# Set TIMEZONE to the TZ environment variable. If TZ is unset, Emacs
# uses system wall clock time, which is a platform-dependent default
# time zone --
# https://www.gnu.org/software/emacs/manual/html_node/elisp/Time-Zone-Rules.html
TIMEZONE=${TZ}

# Port for hugo server
PORT=1337

USE_PACKAGE_DOC_DIR=$(shell pwd)
USE_PACKAGE_DOC_SITE_DIR=$(shell pwd)

# https://stackoverflow.com/a/3774731/1219634
# Note that the use of immediate assignment := rather than recursive
# assignment = is important here: you do not want to be running the
# shell escape every time SOURCES is inspected by make.
org_files := $(shell find ../ -type f -name '*.org')

# Path to the Org file (relative to pwd, or absolute)
# ORG_FILE=

# # Function to be run in emacs --batch
# FUNC=

.PHONY: emacs-batch md1 vcheck hugo doc_site
	md $(org_files) \
	doc_md doc_gh doc \
	ctemp clean

# Note: The Org file from $(ORG_FILE) is loaded *after* the --eval
# section gets evaluated i.e. --eval '(progn ..)' $(ORG_FILE) If the
# order is reversed i.e. i.e.$(ORG_FILE) --eval '(progn ..)', the act
# of loading the $(ORG_FILE) file first will load the older Org
# version that ships with Emacs and then run the stuff in --eval that
# loads the new Org version.. and thus we'll end up with mixed Org in
# the load-path.
emacs-batch:
	@echo ""
	@echo "$(ORG_FILE) ::"
	@$(EMACS) --batch --eval "(progn\
	(setenv \"OX_HUGO_ELPA\" \"$(OX_HUGO_ELPA)\")\
	(when (> (length \"$(TIMEZONE)\") 0) (setenv \"TZ\" \"$(TIMEZONE)\"))\
	(setq-default make-backup-files nil)\
	(load-file (expand-file-name \"setup-ox-hugo.el\" \"$(USE_PACKAGE_DOC_DIR)\"))\
	)" $(ORG_FILE) \
	-f $(FUNC) \
	--kill

md1:
	@$(MAKE) emacs-batch FUNC=org-hugo-export-all-wim-to-md

vcheck:
ifeq ("$(EMACS_exists)","")
	@curl -fsSkL --retry 9 --retry-delay 9 -O $(EMACS_BIN_SOURCE)/emacs-bin-$(EMACS_VERSION).tar.gz
	@tar xf emacs-bin-$(EMACS_VERSION).tar.gz -C /
endif
	@echo "Emacs binary used: $(EMACS)"
	@$(EMACS) --batch --eval "(progn\
	(setenv \"OX_HUGO_ELPA\" \"$(OX_HUGO_ELPA)\")\
	(load-file (expand-file-name \"setup-ox-hugo.el\" \"$(USE_PACKAGE_DOC_DIR)\"))\
	(message \"[Version check] Emacs %s\" emacs-version)\
	(message \"[Version check] %s\" (org-version nil :full))\
	)" \
	--kill
ifeq ("$(HUGO_exists)","")
	@curl https://github.com/gohugoio/hugo/releases/download/v$(HUGO_VERSION)/hugo_$(HUGO_VERSION)_$(HUGO_OS).tar.gz -L --create-dirs -o $(OX_HUGO_ELPA)hugo.tar.gz
	@tar xf $(OX_HUGO_ELPA)hugo.tar.gz -C $(OX_HUGO_ELPA)
	@rm -f $(OX_HUGO_ELPA)hugo.tar.gz
endif
	@$(HUGO) version

hugo: vcheck
	@cd $(HUGO_BASE_DIR) && $(HUGO) --baseURL=$(HUGO_BASE_URL) $(HUGO_ARGS)

doc_site:
	@$(MAKE) hugo HUGO_BASE_DIR=.

serve server: vcheck
	@echo "Serving the site on $(HUGO_BASE_URL):$(PORT) .."
	@cd $(HUGO_BASE_DIR) && $(HUGO) server --baseURL=$(HUGO_BASE_URL) --port $(PORT) --buildDrafts --buildFuture --navigateToChanged

# Run the md1 rules in loop on all of $(org_files)
# https://stackoverflow.com/a/37748952/1219634
md: $(org_files)
$(org_files):
	@$(MAKE) md1 ORG_FILE=$@ TIMEZONE=UTC # Use UTC/Universal time zone for tests

doc_md:
	@echo "[Doc Site] Generating use-package Documentation Site content .."
	@$(MAKE) md1 ORG_FILE=../use-package.org
	@echo "[Doc Site] Done"

# doc_gh:
# 	@echo "[GitHub Docs] Generating README.org and CONTRIBUTING.org for GitHub .."
# 	@$(MAKE) emacs-batch FUNC=use-package-export-gh-doc ORG_FILE=./doc/github-files.org
# 	@echo "[GitHub Docs] Done"

doc: doc_md doc_site

ctemp:
	@find $(USE_PACKAGE_DOC_SITE_DIR)/content -name "*.*~" -delete

clean: ctemp
	@find ./content -name "*.md" -delete
	@find ./content -name "issues" -delete
	@rm -rf $(USE_PACKAGE_DOC_SITE_DIR)/public
	@rm -rf $(OX_HUGO_ELPA)

# Set a make variable during rule execution
# https://stackoverflow.com/a/1909390/1219634

# Check if an executable exists
# https://stackoverflow.com/a/34756868/1219634
