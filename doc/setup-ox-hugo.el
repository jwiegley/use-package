;; Time-stamp: <2017-12-19 16:49:24 kmodi>

;; Setup to export Org files to Hugo-compatible Markdown using
;; `ox-hugo' in an "emacs -Q" environment.

;; Some sane settings
(setq-default require-final-newline t)
(setq-default indent-tabs-mode nil)

(defvar ox-hugo-test-setup-verbose nil
  "When non-nil, enable printing more messages from setup-ox-hugo.el.")

(defvar ox-hugo-install-org-from-elpa (version< emacs-version "26.0")
  "When non-nil, install Org from Org Elpa.

The default value of this variable is non-nil if emacs version is
older than 26, and nil otherwise.

Emacs 26 onwards comes with at least Org 9.1.4.  So there is no
need to install Org from Elpa as that Org version meets the
minimum requirement for `ox-hugo', and thus the doc site
generation.")

(defvar ox-hugo-elpa (let ((dir (getenv "OX_HUGO_ELPA")))
                       (unless dir
                         (setq dir (concat (file-name-as-directory
                                            (concat temporary-file-directory (getenv "USER")))
                                           "ox-hugo-site/")))
                       (setq dir (file-name-as-directory dir))
                       (make-directory dir :parents)
                       dir))
(when ox-hugo-test-setup-verbose
  (message "ox-hugo-elpa: %s" ox-hugo-elpa))

(defvar ox-hugo-packages '(;; toc-org ;No need to install toc-org yet, needed only if exporting to README.org
                           ox-hugo))
(when ox-hugo-install-org-from-elpa
  ;; `org' will always be detected as installed, so use
  ;; `org-plus-contrib'.
  ;; Fri Sep 22 18:24:19 EDT 2017 - kmodi
  ;; Install the packages in the specified order. We do not want
  ;; `toc-org' to be installed first. If that happens, `org' will be
  ;; required before the newer version of Org gets installed and we
  ;; will end up with mixed Org version.  So put `org-plus-contrib' at
  ;; the beginning of `ox-hugo-packages'.
  (add-to-list 'ox-hugo-packages 'org-plus-contrib))

(defvar ox-hugo-site-git-root (progn
                                (require 'vc-git)
                                (file-truename (vc-git-root default-directory)))
  "Absolute path of the git root of the current project.")
(when ox-hugo-test-setup-verbose
  (message "ox-hugo-site-git-root: %S" ox-hugo-site-git-root))

;; Below will prevent installation of `org' package as a dependency
;; when installing `ox-hugo' from Melpa.
(defun ox-hugo-package-dependency-check-ignore (orig-ret)
  "Remove the `black listed packages' from ORIG-RET.

Packages listed in the let-bound `pkg-black-list' will not be auto-installed
even if they are found as dependencies."
  (let ((pkg-black-list '(org))
        new-ret
        pkg-name)
    (dolist (pkg-struct orig-ret)
      (setq pkg-name (package-desc-name pkg-struct))
      (if (member pkg-name pkg-black-list)
          (message (concat "Package `%s' will not be installed. "
                           "See `ox-hugo-package-dependency-check-ignore'.")
                   pkg-name)
        ;; (message "Package to be installed: %s" pkg-name)
        (push pkg-struct new-ret)))
    (setq new-ret (reverse new-ret))
    ;; (message "after  %S" new-ret)
    new-ret))
(advice-add 'package-compute-transaction :filter-return #'ox-hugo-package-dependency-check-ignore)
;; (advice-remove 'package-compute-transaction #'ox-hugo-package-dependency-check-ignore)

(if (and (stringp ox-hugo-elpa)
         (file-exists-p ox-hugo-elpa))
    (progn
      ;; Load newer version of .el and .elc if both are available
      (setq load-prefer-newer t)

      (setq package-user-dir (format "%selpa_%s/" ox-hugo-elpa emacs-major-version))

      ;; Below require will auto-create `package-user-dir' it doesn't exist.
      (require 'package)

      (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                          (not (gnutls-available-p))))
             (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
        (add-to-list 'package-archives (cons "melpa" url)))

      ;; Even if we don't need to install Org from Elpa, we need to
      ;; add Org Elpa in `package-archives' to prevent the "Package
      ;; ‘org-9.0’ is unavailable" error.
      (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/")) ;For latest stable `org'

      ;; Load emacs packages and activate them.
      ;; Don't delete this line.
      (package-initialize)
      ;; `package-initialize' call is required before any of the below
      ;; can happen.
      ;; (add-to-list 'load-path (concat ox-hugo-site-git-root "doc/")) ;For ox-hugo-export-gh-doc.el

      (defvar ox-hugo-missing-packages '()
        "List populated at each startup that contains the list of packages that need
to be installed.")

      (dolist (p ox-hugo-packages)
        ;; (message "Is %S installed? %s" p (package-installed-p p))
        (unless (package-installed-p p)
          (add-to-list 'ox-hugo-missing-packages p :append)))

      (when ox-hugo-missing-packages
        (message "Emacs is now refreshing its package database...")
        (package-refresh-contents)
        ;; Install the missing packages
        (dolist (p ox-hugo-missing-packages)
          (message "Installing `%s' .." p)
          (package-install p))
        (setq ox-hugo-missing-packages '())))
  (error "The environment variable OX_HUGO_ELPA needs to be set"))

;; Remove Org that ships with Emacs from the `load-path' if installing
;; it from Elpa.
(when ox-hugo-install-org-from-elpa
  (let* ((bin-dir (when (and invocation-directory
                             (file-exists-p invocation-directory))
                    (file-truename invocation-directory)))
         (prefix-dir (when bin-dir
                       (replace-regexp-in-string "bin/\\'" "" bin-dir)))
         (share-dir (when prefix-dir
                      (concat prefix-dir "share/")))
         (lisp-dir-1 (when share-dir ;Possibility where the lisp dir is something like ../emacs/26.0.50/lisp/
                       (concat share-dir "emacs/"
                               ;; If `emacs-version' is x.y.z.w, remove the ".w" portion
                               ;; Though, this is not needed and also will do nothing in emacs 26+
                               ;; http://git.savannah.gnu.org/cgit/emacs.git/commit/?id=22b2207471807bda86534b4faf1a29b3a6447536
                               (replace-regexp-in-string "\\([0-9]+\\.[0-9]+\\.[0-9]+\\).*" "\\1" emacs-version)
                               "/lisp/")))
         (lisp-dir-2 (when share-dir ;Possibility where the lisp dir is something like ../emacs/25.2/lisp/
                       (concat share-dir "emacs/"
                               (replace-regexp-in-string "\\([0-9]+\\.[0-9]+\\).*" "\\1" emacs-version)
                               "/lisp/"))))
    (when ox-hugo-test-setup-verbose
      (message "emacs bin-dir: %s" bin-dir)
      (message "emacs prefix-dir: %s" prefix-dir)
      (message "emacs share-dir: %s" share-dir)
      (message "emacs lisp-dir-1: %s" lisp-dir-1)
      (message "emacs lisp-dir-2: %s" lisp-dir-2))
    (defvar ox-hugo-default-lisp-directory (cond
                                            ((file-exists-p lisp-dir-1)
                                             lisp-dir-1)
                                            ((file-exists-p lisp-dir-2)
                                             lisp-dir-2)
                                            (t
                                             nil))
      "Directory containing lisp files for the Emacs installation.

This value must match the path to the lisp/ directory of the
Emacs installation.  If Emacs is installed using
--prefix=\"${PREFIX_DIR}\" this value would typically be
\"${PREFIX_DIR}/share/emacs/<VERSION>/lisp/\"."))
  (when ox-hugo-test-setup-verbose
    (message "ox-hugo-default-lisp-directory: %S" ox-hugo-default-lisp-directory))

  (with-eval-after-load 'package
    ;; Remove Org that ships with Emacs from the `load-path'.
    (when (stringp ox-hugo-default-lisp-directory)
      (dolist (path load-path)
        (when (string-match-p (expand-file-name "org" ox-hugo-default-lisp-directory) path)
          (setq load-path (delete path load-path))))))

  ;; (message "`load-path': %S" load-path)
  ;; (message "`load-path' Shadows:")
  ;; (message (list-load-path-shadows :stringp))
  )

(require 'ox-hugo)
(defun org-hugo-export-all-wim-to-md ()
  (org-hugo-export-wim-to-md :all-subtrees nil nil :noerror))

;; (require 'ox-hugo-export-gh-doc)        ;For `ox-hugo-export-gh-doc'

(with-eval-after-load 'org
  ;; Allow multiple line Org emphasis markup
  ;; http://emacs.stackexchange.com/a/13828/115
  (setcar (nthcdr 4 org-emphasis-regexp-components) 20) ;Up to 20 lines, default is just 1
  ;; Below is needed to apply the modified `org-emphasis-regexp-components'
  ;; settings from above.
  (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)

  ;; Prevent prompts like:
  ;;   Non-existent agenda file
  (defun org-check-agenda-file (file))

  (let (ob-lang-alist)
    (add-to-list 'ob-lang-alist '(emacs-lisp . t))
    (add-to-list 'ob-lang-alist '(org . t))
    (org-babel-do-load-languages 'org-babel-load-languages ob-lang-alist))

  (with-eval-after-load 'ob-core
    (defun ox-hugo-org-confirm-babel-evaluate-fn (lang body)
      "Mark `org' as a safe language for ox-hugo tests and docs."
      (let* ((ob-enabled-langs '("org"))
             (ob-enabled-langs-re (regexp-opt ob-enabled-langs 'words))
             (unsafe t)) ;Set the return value `unsafe' to t by default
        (when (string-match-p ob-enabled-langs-re lang)
          (setq unsafe nil))
        unsafe))
    (setq org-confirm-babel-evaluate #'ox-hugo-org-confirm-babel-evaluate-fn))

  (with-eval-after-load 'ox
    (setq org-export-headline-levels 4) ;default is 3
    (add-to-list 'org-export-exclude-tags "ignore")))
