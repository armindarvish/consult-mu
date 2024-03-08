;;; consult-mu-compose-embark.el --- Emabrk Actions for consult-mu-compose -*- lexical-binding: t -*-

;; Copyright (C) 2021-2023

;; Author: Armin Darvish
;; Maintainer: Armin Darvish
;; Created: 2023
;; Version: 1.0
;; Package-Requires: ((emacs "28.0") (consult "0.34"))
;; Homepage: https://github.com/armindarvish/consult-mu
;; Keywords: convenience, matching, tools, email

;;; Commentary:

;;; Code:

;;; Requirements
(require 'embark)
(require 'consult-mu)
(require 'consult-mu-embark)

(defun consult-mu-compose-embark-attach-file (cand)
  "Run `consult-mu-attach-files' on the candidate."
    (funcall (apply-partially #'consult-mu-compose-attach cand)))

;;; add consult-mu-attach to embark-file-map
(defun consult-mu-compose-embark-bind-attach-file-key (&optional key)
"Binds `consult-mu-embark-attach-file-key' to `consult-mu-compose-embark-attach-file' in `embark-file-map'.

If KEY is non-nil binds KEY instead of `consult-mu-embark-attach-file-key'."
(if-let ((keyb (or key (kbd consult-mu-embark-attach-file-key))))
(define-key embark-file-map keyb #'consult-mu-compose-embark-attach-file)))

(consult-mu-compose-embark-bind-attach-file-key)

;; change the default action on `consult-mu-contacts category.
(add-to-list 'embark-default-action-overrides '((file . consult-mu-compose--read-file-attach)  . consult-mu-compose-attach))
(add-to-list 'embark-default-action-overrides '((file . consult-mu-compose-attach)  . consult-mu-compose-attach))

;;; Provide `consult-mu-compose-embark' module

(provide 'consult-mu-compose-embark)

;;;  consult-mu-compose-embark.el ends here
