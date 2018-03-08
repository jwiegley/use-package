;;; use-package-hydra.el --- Support for the :hydra keyword

;; Copyright (C) 2012-2018 John Wiegley

;; Author: Toon Claes <toon@iotcl.com>
;; Maintainer: John Wiegley <johnw@newartisans.com>
;; Created: 6 Jan 2018
;; Modified: 18 Feb 2018
;; Version: 1.0
;; Package-Requires: ((emacs "24.3") (use-package "2.4"))
;; Keywords: dotemacs startup speed config package
;; URL: https://github.com/jwiegley/use-package

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
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

;; Provides support for the :hydra keyword, which is made available by
;; default by requiring `use-package'.

;;; Code:

(require 'use-package-core)

(defun use-package-hydra--name (name)
  "Build hydra name for the package NAME."
  (make-symbol (cl-gentemp (concat "hydra-" name))))

;;;###autoload
(defun use-package-hydra--normalize (name _keyword args)
  "Normalize the ARGS to be a list hydras.
It accepts a single hydra, or a list of hydras.  It is optional
provide a name for the hydra, if so there is a name generated
from NAME."
  (let ((arg args)
        args*)
    (while arg
      (let ((x (car arg)))
        (cond
         ;; single named hydra
         ((symbolp (car x))
          (setq args* (nconc args* (list x)))
          (setq arg (cdr arg)))
         ;; single unnamed hydra with docstring
         ((stringp (nth 2 x))
          (use-package-hydra--name name)
          (setq args* (nconc args* (list x)))
          (setq arg (cdr arg)))
         ;; single unnamed hydra without docstring
         (something
          )

         ;; list of hydras
         ((listp x)
          (setq args*
                (nconc args* (use-package-hydra--normalize name _keyword x)))
          (setq arg (cdr arg)))
         ;; Error!
         (t
          (use-package-error
           (concat (symbol-name name)
                   " wants arguments acceptable to the `defhydra' macro,"
                   " or a list of such values"))))))
    args*))

;;;###autoload
(defalias 'use-package-normalize/:hydra 'use-package-hydra--normalize
  "Normalize for the definition of one or more hydras.")

(defun use-package-normalize/:hydra (name keyword args)
  "Normalize for the definition of one or more hydras."
  (let ((arg args)
        args*)
    (while arg
      (let ((x (car arg)))
        (unless (listp x)
          (use-package-error
 ;;                 " wants arguments acceptable to the `bind-keys' macro,"
;;                   " or a list of such values"))))))
           (concat label " a ([hydra-name] <body> <docstring> <heads>)"
                   " or list of these")))
        (setq arg (cdr arg))
        (setq args* (nconc args* (if (symbolp (car x))
                                     x
                                   (list (use-package-hydra--name name x)))))))))



;;;###autoload
(defun use-package-handler/:hydra (name keyword args rest state)
  "Generate defhydra code for hydra keyword."
  (use-package-concat
   (mapcar
    #'(lambda (def)
        (let ((name (nth 0 def))
              (body (nth 1 def))
              (docstring (nth 2 def))
              (heads (nthcdr 2 def)))
          `(defhydra ,name ,body ,docstring ,heads)))
    args)
   (use-package-process-keywords name rest state)))

(add-to-list 'use-package-keywords :hydra t)

;; REMOVE ME
(when nil

(use-package use-package
  :hydra (hydra-single (global-map "<f3>")
                       "Single"
                       ("g" text-scale-increase "Single +")
                       ("l" text-scale-decrease "Single -")))


(use-package use-package
  :hydra ((hydra-first (global-map "<f4>")
                       "First"
                       ("g" text-scale-increase "First +")
                       ("l" text-scale-decrease "First -"))
          (hydra-second (global-map "<f5>")
                        "Second"
                        ("g" text-scale-increase "Second +")
                        ("l" text-scale-decrease "Second -"))))



(defhydra hydra-toon
         (global-map "<f2>")
         "zoom"
         ("g" text-scale-increase "in"))


)
;; REMOVE ME



(provide 'use-package-hydra)
;;; use-package-hydra.el ends here
