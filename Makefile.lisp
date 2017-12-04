# -*- Makefile -*-

-include config.mk
include default.mk

## ###################################################################

.PHONY: install

all: lisp

## Build order #######################################################

use-package.elc:	bind-key.elc

## Build #############################################################

lisp:       $(ELCS)

%.elc: %.el
	@printf "Compiling $<\n"
	-@$(BATCH) --eval "(progn\
	(when (file-exists-p \"$@\")\
	  (delete-file \"$@\"))\
	(fset 'message* (symbol-function 'message))\
	(fset 'message  (lambda (f &rest a)\
	                  (unless (equal f \"Wrote %s\")\
	                    (apply 'message* f a)))))" \
	-f batch-byte-compile $<

## Install ###########################################################

install: lisp
	@$(MKDIR) $(DESTDIR)$(lispdir)
	$(CP) $(ELS) $(ELCS) $(DESTDIR)$(lispdir)

## Clean #############################################################

clean:
	@printf "Cleaning lisp...\n"
	$(RM) *.elc $(ELGS)
