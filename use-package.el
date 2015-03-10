;;; use-package.el --- A use-package declaration for simplifying your .emacs

;; Copyright (C) 2012 John Wiegley

;; Author: John Wiegley <jwiegley@gmail.com>
;; Created: 17 Jun 2012
;; Version: 1.0
;; Package-Requires: ((bind-key "1.0") (diminish "0.44"))
;; Keywords: dotemacs startup speed config package
;; X-URL: https://github.com/jwiegley/use-package

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; The `use-package' declaration macro allows you to isolate package
;; configuration in your ".emacs" in a way that is performance-oriented and,
;; well, just tidy.  I created it because I have over 80 packages that I use
;; in Emacs, and things were getting difficult to manage.  Yet with this
;; utility my total load time is just under 1 second, with no loss of
;; functionality!
;;
;; Please see README.md from the same repository for documentation.

;;; Code:

(require 'bind-key)
(require 'bytecomp)
(require 'diminish nil t)

(declare-function package-installed-p 'package)

(defgroup use-package nil
  "A use-package declaration for simplifying your `.emacs'."
  :group 'startup)

(defcustom use-package-verbose nil
  "Whether to report about loading and configuration details.

If you customize this, then you should require the `use-package'
feature in files that use one of the macros `use-package' or
`use-package-with-elapsed-timer', even if these files only
contain compiled expansions of the macros.  If you don't do so,
then the expanded macros do their job silently."
  :type 'boolean
  :group 'use-package)

(defcustom use-package-minimum-reported-time 0.01
  "Minimal load time that will be reported.

Note that `use-package-verbose' has to be set to t, for anything
to be reported at all.

If you customize this, then you should require the `use-package'
feature in files that use one of the macros `use-package' or
`use-package-with-elapsed-timer', even if these files only
contain compiled expansions of the macros.  If you don't do so,
then the expanded macros do their job silently."
  :type 'number
  :group 'use-package)

(defcustom use-package-idle-interval 3
  "Time to wait when using :idle in a `use-package' specification."
  :type 'number
  :group 'use-package)

(defmacro use-package-with-elapsed-timer (text &rest body)
  (declare (indent 1))
  (let ((nowvar (make-symbol "now")))
    `(if (bound-and-true-p use-package-verbose)
         (let ((,nowvar (current-time)))
           (message "%s..." ,text)
           (prog1 (progn ,@body)
             (let ((elapsed
                    (float-time (time-subtract (current-time) ,nowvar))))
               (if (> elapsed
                      (or (bound-and-true-p use-package-minimum-reported-time)
                          "0.01"))
                   (message "%s...done (%.3fs)" ,text elapsed)
                 (message "%s...done" ,text)))))
       ,@body)))

(put 'use-package-with-elapsed-timer 'lisp-indent-function 1)

(defvar use-package-idle-timer nil)
(defvar use-package-idle-forms (make-hash-table))

(defun use-package-start-idle-timer ()
  "Ensure that the idle timer is running."
  (unless use-package-idle-timer
    (setq use-package-idle-timer
          (run-with-idle-timer
           use-package-idle-interval t
           'use-package-idle-eval))))

(defun use-package-init-on-idle (form priority)
  "Add a new form to the idle queue."
  (use-package-start-idle-timer)
  (puthash priority
           (append (gethash priority use-package-idle-forms)
                   (list form))
           use-package-idle-forms))

(defun use-package-idle-priorities ()
  "Get a list of all priorities in the idle queue.
The list is sorted in the order forms should be run."
  (let ((priorities nil))
    (maphash (lambda (priority forms)
               (setq priorities (cons priority priorities)))
             use-package-idle-forms)
    (sort priorities '<)))

(defun use-package-idle-pop ()
  "Pop the top-priority task from the idle queue.
Return nil when the queue is empty."
  (let* ((priority        (car (use-package-idle-priorities)))
         (forms           (gethash priority use-package-idle-forms))
         (first-form      (car forms))
         (forms-remaining (cdr forms)))
    (if forms-remaining
        (puthash priority forms-remaining use-package-idle-forms)
      (remhash priority use-package-idle-forms))
    first-form))

(defun use-package-idle-eval()
  "Start to eval idle-commands from the idle queue."
  (let ((next (use-package-idle-pop)))
    (if next
        (progn
          (when use-package-verbose
            (message "use-package idle:%s" next))

          (condition-case e
              (funcall next)
            (error
             (message
              "Failure on use-package idle. Form: %s, Error: %s"
              next e)))
          ;; recurse after a bit
          (when (sit-for use-package-idle-interval)
            (use-package-idle-eval)))
      ;; finished (so far!)
      (cancel-timer use-package-idle-timer)
      (setq use-package-idle-timer nil))))

(defun use-package-pin-package (package archive)
  "Pin PACKAGE to ARCHIVE."
  (unless (boundp 'package-pinned-packages)
    (setq package-pinned-packages ()))
  (let ((archive-symbol (if (symbolp archive) archive (intern archive)))
        (archive-name   (if (stringp archive) archive (symbol-name archive))))
    (if (use-package--archive-exists-p archive-symbol)
        (add-to-list 'package-pinned-packages (cons package archive-name))
      (error (message  "Archive '%s' requested for package '%s' is not available." archive-name package)))
    (package-initialize t)))

(defun use-package--archive-exists-p (archive)
  "Check if a given ARCHIVE is enabled.

ARCHIVE can be a string or a symbol or 'manual to indicate a manually updated package."
  (if (member archive '(manual "manual"))
      't
    (let ((valid nil))
      (dolist (pa package-archives)
        (when (member archive (list (car pa) (intern (car pa))))
          (setq valid 't)))
      valid)))

(defun use-package-ensure-elpa (package)
  (when (not (package-installed-p package))
    (package-install package)))

(defvar use-package-keywords
  '(
    :bind
    :bind*
    :commands
    :config
    :defer
    :defines
    :demand
    :diminish
    :disabled
    :ensure
    :idle
    :idle-priority
    :if
    :init
    :interpreter
    :load-path
    :mode
    :pin
    :pre-init
    :pre-load
    :requires
    :bind-keymap
    :bind-keymap*
    )
  "Keywords recognized by `use-package'.")

(defun use-package-mplist-get (plist prop)
  "Get the values associated to PROP in PLIST, a modified plist.

A modified plist is one where keys are keywords and values are
all non-keywords elements that follow it.

As a special case : if the first occurrence of the keyword PROP
is followed by another keyword or is the last element in the
list, the function returns t.

Currently this function infloops when the list is circular."
  (let ((tail plist)
        found
        result)
    (while (and
            (consp tail)
            (not
             (eq prop (car tail))))
      (pop tail))
    (when (eq prop (pop tail))
      (setq found t))
    (while (and (consp tail)
                (not (keywordp (car tail))))
      (push (pop tail) result))
    (or (nreverse result) found)))

(defun use-package-plist-get (plist prop &optional eval-backquote no-progn)
  "Compatibility layer between classical and modified plists.

If `use-package-mplist-get' returns exactly one value, that is
returned ; otherwise the list is returned wrapped in a `progn'
unless NO-PROGN is non-nil.

When EVAL-BACKQUOTE is non-nil, the value is first evaluated as
if it were backquoted."
  (let ((values (use-package-mplist-get plist prop)))
    (when eval-backquote
      (setq values (eval (list 'backquote values))))
    (when values
      (cond ((not (listp values))
             values)
            ((eq 1 (length values))
             (car values))
            (t (if no-progn
                   values
                 (cons 'progn values)))))))

(defun use-package-mplist-keys (plist)
  "Get the keys in PLIST, a modified plist.

A modified plist is one where properties are keywords and values
are all non-keywords elements that follow it."
  (let ((result))
    (mapc (lambda (elt)
            (when (keywordp elt)
              (push elt result)))
          plist)
    (nreverse result)))

(defun use-package-validate-keywords (args)
  "Error if any keyword given in ARGS is not recognized.
Return the list of recognized keywords."
  (mapc
   (function
    (lambda (keyword)
      (unless (memq keyword use-package-keywords)
        (error "Unrecognized keyword: %s" keyword))))
   (use-package-mplist-keys args)))

(defmacro use-package (name &rest args)
  "Use a package with configuration options.

For full documentation. please see commentary.

  (use-package package-name
     :keyword option)

:init Code to run when `use-package' form evals.
:bind Perform key bindings, and define autoload for bound
      commands.
:bind* Perform key bindings, and define autoload for bound
      commands, overriding all minor mode bindings.
:bind-keymap Bind key prefix to an auto-loaded keymap that
      is defined in the package.  Like bind but for keymaps
      instead of commands.
:bind-keymap* like bind-keymap, but overrides all minor mode bindings
:commands Define autoloads for given commands.
:pre-load Code to run when `use-package' form evals and before
       anything else. Unlike :init this form runs before the
       package is required or autoloads added.
:mode Form to be added to `auto-mode-alist'.
:interpreter Form to be added to `interpreter-mode-alist'.
:defer Defer loading of package -- automatic
       if :commands, :bind, :bind*, :mode or :interpreter are used.
:demand Prevent deferred loading in all cases.
:config Runs if and when package loads.
:if Conditional loading.
:disabled Ignore everything.
:defines Define vars to silence byte-compiler.
:load-path Add to `load-path' before loading.
:diminish Support for diminish package (if it's installed).
:idle adds a form to run on an idle timer
:idle-priority schedules the :idle form to run with the given
       priority (lower priorities run first). Default priority
       is 5; forms with the same priority are run in the order in
       which they are evaluated.
:ensure loads package using package.el if necessary.
:pin pin package to archive."
  (use-package-validate-keywords args) ; error if any bad keyword, ignore result
  (let* ((commands (use-package-plist-get args :commands t t))
         (pre-init-body (use-package-plist-get args :pre-init))
         (pre-load-body (use-package-plist-get args :pre-load))
         (init-body (use-package-plist-get args :init))
         (config-body (use-package-plist-get args :config))
         (diminish-var (use-package-plist-get args :diminish t))
         (defines (use-package-plist-get args :defines t t))
         (idle-body (use-package-plist-get args :idle))
         (idle-priority (use-package-plist-get args :idle-priority))
         (keybindings-alist (use-package-plist-get args :bind t t))
         (overriding-keybindings-alist (use-package-plist-get args :bind* t t))
         (keymap-alist (use-package-plist-get args :bind-keymap t t))
         (overriding-keymap-alist (use-package-plist-get args :bind-keymap* t t))
         (mode (use-package-plist-get args :mode t t))
         (mode-alist
          (if (stringp mode) (cons mode name) mode))
         (interpreter (use-package-plist-get args :interpreter t t))
         (interpreter-alist
          (if (stringp interpreter) (cons interpreter name) interpreter))
         (predicate (use-package-plist-get args :if))
         (pkg-load-path (use-package-plist-get args :load-path t t))
         (archive-name (use-package-plist-get args :pin))
         (defines-eval (if (null defines)
                           nil
                         (if (listp defines)
                             (mapcar (lambda (var) `(defvar ,var)) defines)
                           `((defvar ,defines)))))
         (requires (use-package-plist-get args :requires t))
         (requires-test (if (null requires)
                            t
                          (if (listp requires)
                              `(not (member nil (mapcar #'featurep
                                                        (quote ,requires))))
                            `(featurep (quote ,requires)))))
         (name-string (if (stringp name) name (symbol-name name)))
         (name-symbol (if (stringp name) (intern name) name)))

    ;; force this immediately -- one off cost
    (unless (eval (use-package-plist-get args :disabled))

      (when archive-name
        (use-package-pin-package name archive-name))

      (let* ((ensure (use-package-plist-get args :ensure))
             (package-name
              (or (and (eq ensure t)
                       name)
                  ensure)))

        (when package-name
          (require 'package)
          (use-package-ensure-elpa package-name)))


      (if diminish-var
          (setq config-body
                `(progn
                   ,config-body
                   (ignore-errors
                     ,@(cond
                        ((stringp diminish-var)
                         `((diminish (quote ,(intern (concat name-string "-mode")))
                                     ,diminish-var)))
                        ((symbolp diminish-var)
                         `((diminish (quote ,diminish-var))))
                        ((and (consp diminish-var) (stringp (cdr diminish-var)))
                         `((diminish (quote ,(car diminish-var)) ,(cdr diminish-var))))
                        (t      ; list of symbols or (symbol . "string") pairs
                         (mapcar (lambda (var)
                                   (if (listp var)
                                       `(diminish (quote ,(car var)) ,(cdr var))
                                     `(diminish (quote ,var))))
                                 diminish-var)))))))

      (if (and commands (symbolp commands))
          (setq commands (list commands)))


      (when idle-body
        (when (null idle-priority)
          (setq idle-priority 5))
        (setq init-body
              `(progn
                 (require 'use-package)
                 (use-package-init-on-idle (lambda () ,idle-body) ,idle-priority)
                 ,init-body)))

      (let ((init-for-commands-or-keymaps
             (lambda (func sym-or-list &optional keymap)
               (let ((cons-list (if (and (consp sym-or-list)
                                         (stringp (car sym-or-list)))
                                    (list sym-or-list)
                                  sym-or-list)))
                 (if cons-list
                     (setq init-body
                           `(progn
                              ,init-body
                              ,@(mapcar (lambda (elem)
                                          (when (not keymap)
                                              (push (cdr elem) commands))
                                          (funcall func elem))
                                        cons-list))))))))

        (funcall init-for-commands-or-keymaps
                 (lambda (binding)
                   `(bind-key ,(car binding)
                              (lambda () (interactive)
                                (use-package-autoload-keymap
                                 (quote ,(cdr binding))
                                 ,(if (stringp name) name `',name)
                                 nil))))
                 keymap-alist
                 t)

        (funcall init-for-commands-or-keymaps
                 (lambda (binding)
                   `(bind-key ,(car binding)
                              (lambda () (interactive)
                                (use-package-autoload-keymap
                                 (quote ,(cdr binding))
                                 ,(if (stringp name) name `',name)
                                 t))))
                 overriding-keymap-alist
                 t)

        (funcall init-for-commands-or-keymaps
                 (lambda (binding)
                   `(bind-key ,(car binding)
                              (quote ,(cdr binding))))
                 keybindings-alist)

        (funcall init-for-commands-or-keymaps
                 (lambda (binding)
                   `(bind-key* ,(car binding)
                               (quote ,(cdr binding))))
                 overriding-keybindings-alist)

        (funcall init-for-commands-or-keymaps
                 (lambda (mode)
                   `(add-to-list 'auto-mode-alist
                                 (quote ,mode)))
                 mode-alist)

        (funcall init-for-commands-or-keymaps
                 (lambda (interpreter)
                   `(add-to-list 'interpreter-mode-alist
                                 (quote ,interpreter)))
                 interpreter-alist))

      `(progn
         ,pre-load-body
         ,@(mapcar
            (lambda (path)
              `(add-to-list 'load-path
                            ,(if (file-name-absolute-p path)
                                 path
                               (expand-file-name path user-emacs-directory))))
            (cond ((stringp pkg-load-path)
                   (list pkg-load-path))
                  ((functionp pkg-load-path)
                   (funcall pkg-load-path))
                  (t
                   pkg-load-path)))

         (eval-when-compile
           (when (bound-and-true-p byte-compile-current-file)
             ,@defines-eval
             (condition-case err
                 ,(if (stringp name)
                      `(load ,name t)
                    `(require ',name nil t))
               (error (message "Error requiring %s: %s" ',name err) nil))))

         ,(if (and (or commands (use-package-plist-get args :defer)
                       (use-package-plist-get args :bind-keymap)
                       (use-package-plist-get args :bind-keymap*))
                   (not (use-package-plist-get args :demand)))
              (let (form)
                (mapc (lambda (command)
                   (push `(unless (fboundp (quote ,command))
                            (autoload (function ,command)
                              ,name-string nil t))
                         form))
                      commands)

                `(when ,(or predicate t)
                   ,pre-init-body
                   ,@form
                   ,init-body
                   ,(unless (null config-body)
                      `(eval-after-load ,(if (stringp name) name `',name)
                         `(,(lambda ()
                              (if ,requires-test
                                  (use-package-with-elapsed-timer
                                      ,(format "Configuring package %s" name-string)
                                    ,config-body))))))
                   t))
            `(if (and ,(or predicate t)
                      ,requires-test)
                 (use-package-with-elapsed-timer
                     ,(format "Loading package %s" name-string)
                   (if (not ,(if (stringp name)
                                 `(load ,name t)
                               `(require ',name nil t)))
                       (message "Could not load package %s" ,name-string)
                     ,pre-init-body
                     ,init-body
                     ,config-body
                     t))))))))

(defun use-package-autoload-keymap (keymap-symbol package override)
  "Loads PACKAGE and then binds the key sequence used to invoke this function to
KEYMAP-SYMBOL.  It then simulates pressing the same key sequence a again, so
that the next key pressed is routed to the newly loaded keymap.

This function supports use-package's :bind-keymap keyword.  It works
by binding the given key sequence to an invocation of this function for a
particular keymap.  The keymap is expected to be defined by the package.  In
this way, loading the package is deferred until the prefix key sequence is
pressed."
  (if (if (stringp package) (load package t) (require package nil t))
      (if (and (boundp keymap-symbol) (keymapp (symbol-value keymap-symbol)))
          (let ((key (key-description (this-command-keys-vector)))
                (keymap (symbol-value keymap-symbol)))
            (progn
              (if override
                  `(eval `(bind-key* ,key ,keymap)) ; eval form is necessary to avoid compiler error
                (bind-key key keymap))
              (setq unread-command-events
                    (listify-key-sequence (this-command-keys-vector)))))
        (error
         "use-package: package %s failed to define keymap %s"
         package keymap-symbol))
    (error "Could not load package %s" package)))

(put 'use-package 'lisp-indent-function 'defun)

(defconst use-package-font-lock-keywords
  '(("(\\(use-package\\(?:-with-elapsed-timer\\)?\\)\\_>[ \t']*\\(\\(?:\\sw\\|\\s_\\)+\\)?"
     (1 font-lock-keyword-face)
     (2 font-lock-constant-face nil t))))

(font-lock-add-keywords 'emacs-lisp-mode use-package-font-lock-keywords)

(provide 'use-package)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; use-package.el ends here
