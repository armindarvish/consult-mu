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


;;; add attach to file keymap

(define-key embark-file-map "a" #'consult-mu-compose-embark-attach-file)

;; (defvar-keymap consult-mu-embark-compose-actions-map
;;   :doc "Keymap for consult-mu-embark-contacts"
;;   :parent consult-mu-embark-general-actions-map
;;   "a" #'consult-mu-compose-embark-attach-file
;;   )

;; (add-to-list 'embark-keymap-alist '(consult-mu-compose . consult-mu-embark-compose-actions-map))


;; change the default action on `consult-mu-contacts category.
(add-to-list 'embark-default-action-overrides '((file . consult-mu-compose--read-file-attach)  . consult-mu-compose-attach))
(add-to-list 'embark-default-action-overrides '((file . consult-mu-compose-attach)  . consult-mu-compose-attach))

;;; Provide `consult-mu-compose-embark' module

(provide 'consult-mu-compose-embark)

;;;  consult-mu-compose-embark.el ends here
