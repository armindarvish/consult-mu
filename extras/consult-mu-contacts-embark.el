;;; consult-mu-contacts-embark.el --- Emabrk Actions for consult-mu-contacts -*- lexical-binding: t -*-

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

(defun consult-mu-contacts-embark-insert-email (cand)
  "Embark function for inserting CAND's email."
  (let* ((contact (get-text-property 0 :contact cand))
         (email (plist-get contact :email)))
    (insert (concat email "; "))))

(defun consult-mu-contacts-embark-kill-email (cand)
  "Embark function for copying CAND's email."
  (let* ((contact (get-text-property 0 :contact cand))
         (email (plist-get contact :email)))
    (kill-new email)))

(defun consult-mu-contacts-embark-get-alternative (cand)
  "Embark function for copying CAND's email."
  (let* ((contact (get-text-property 0 :contact cand))
         (name (string-trim (plist-get contact :name)))
         (email (plist-get contact :email))
         (user (string-trim (replace-regexp-in-string "@.*" "" email))))
    (consult-mu-contacts (cond
                          ((not (string-empty-p name))
                           name)
                          ((not (string-empty-p user))
                           user)
                          ((t ""))))))

(defun consult-mu-contacts-embark-compose (cand)
  "Embark function for composing an email to CAND."
  (let* ((contact (get-text-property 0 :contact cand)))
    (consult-mu-contacts--compose-to contact)))

(defun consult-mu-contacts-embark-search-messages (cand)
  "Embark function for searching messages from CAND using `consult-mu'."
  (let* ((contact (get-text-property 0 :contact cand))
         (email (plist-get contact :email)))
    (consult-mu (concat "from:" email))))

(defun consult-mu-contacts-embark-default-action (cand)
  "Run `consult-mu-contacts-action' on CAND."
  (let* ((contact (get-text-property 0 :contact cand))
         (query (get-text-property 0 :query cand))
         (newcand (cons cand `(:contact ,contact :query ,query))))
    (funcall #'consult-mu-contacts--insert-email-action newcand)))

;;; Define Embark Keymaps
(defvar-keymap consult-mu-embark-contacts-actions-map
  :doc "Keymap for consult-mu-embark-contacts"
  :parent consult-mu-embark-general-actions-map
  "c" #'consult-mu-contacts-embark-compose
  "s" #'consult-mu-contacts-embark-search-messages
  "i" #'consult-mu-contacts-embark-insert-email
  "w" #'consult-mu-contacts-embark-kill-email
  "a" #'consult-mu-contacts-embark-get-alternative)


(add-to-list 'embark-keymap-alist '(consult-mu-contacts . consult-mu-embark-contacts-actions-map))

;; change the default action on `consult-mu-contacts category.
(add-to-list 'embark-default-action-overrides '(consult-mu-contacts . consult-mu-contacts-embark-default-action))

;;; Provide `consult-mu-contacts-embark' module

(provide 'consult-mu-contacts-embark)

;;; consult-mu-contacts-embark.el ends here
