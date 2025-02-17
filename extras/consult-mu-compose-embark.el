;;; consult-mu-compose-embark.el --- Emabrk Actions for consult-mu-compose -*- lexical-binding: t -*-

;; Copyright (C) 2021-2023

;; Author: Armin Darvish
;; Maintainer: Armin Darvish
;; Created: 2023
;; Version: 1.0
;; Package-Requires: ((emacs "28.0") (consult "2.0"))
;; Homepage: https://github.com/armindarvish/consult-mu
;; Keywords: convenience, matching, tools, email
;; Homepage: https://github.com/armindarvish/consult-mu

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; This package provides an alternative interactive serach interface for
;; mu and mu4e (see URL `https://djcbsoftware.nl/code/mu/mu4e.html').
;; It uses a consult-based minibuffer completion for searching and
;; selecting, and marking emails, as well as additional utilities for
;; composing emails and more.

;;  This package requires mu4e version "1.10.8" or later.


;;; Code:

;;; Requirements
(require 'embark)
(require 'consult-mu)
(require 'consult-mu-embark)

(defun consult-mu-compose-embark-attach-file (cand)
  "Run `consult-mu-attach-files' on CAND."
  (funcall (apply-partially #'consult-mu-compose-attach cand)))

;;; add consult-mu-attach to embark-file-map
(defun consult-mu-compose-embark-bind-attach-file-key (&optional key)
  "Binds `consult-mu-embark-attach-file-key'.

Bind `consult-mu-embark-attach-file-key' to
`consult-mu-compose-embark-attach-file' in `embark-file-map'.  If KEY is
non-nil binds KEY instead of `consult-mu-embark-attach-file-key'."
  (if-let ((keyb (or key (kbd consult-mu-embark-attach-file-key))))
      (define-key embark-file-map keyb #'consult-mu-compose-embark-attach-file)))

(consult-mu-compose-embark-bind-attach-file-key)

;; change the default action on `consult-mu-contacts category.
(add-to-list 'embark-default-action-overrides '((file . consult-mu-compose--read-file-attach)  . consult-mu-compose-attach))
(add-to-list 'embark-default-action-overrides '((file . consult-mu-compose-attach)  . consult-mu-compose-attach))

;;; Provide `consult-mu-compose-embark' module

(provide 'consult-mu-compose-embark)

;;; consult-mu-compose-embark.el ends here
