;;; consult-mu-embark.el --- Emabrk Actions for consult-mu -*- lexical-binding: t -*-

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

;;; Customization Variables
(defcustom consult-mu-embark-noconfirm-before-execute nil
  "Should consult-mu-embark skip confirmation when executing marks?"
  :group 'consult-mu
  :type 'boolean)

;;; Define Embark Action Functions
(defun consult-mu-embark-default-action (cand)
  "Run `consult-mu-action' on the candidate, CAND."
  (let* ((msg (get-text-property 0 :msg cand))
         (query (get-text-property 0 :query cand))
         (type (get-text-property 0 :type cand))
         (newcand (cons cand `(:msg ,msg :query ,query :type ,type))))
    (if (equal type :async)
        (consult-mu--update-headers query t msg :async))
    (funcall consult-mu-action newcand)))



(defun consult-mu-embark-reply (cand)
  "Reply to message in CAND."
  (let* ((msg (get-text-property 0 :msg cand))
         (query (get-text-property 0 :query cand))
         (type (get-text-property 0 :type cand)))
    (if (equal type :async)
        (consult-mu--update-headers query t msg :async))
    (consult-mu--reply msg nil)))

(defun consult-mu-embark-wide-reply (cand)
  "Reply all for message in CAND."
  (let* ((msg (get-text-property 0 :msg cand))
         (query (get-text-property 0 :query cand))
         (type (get-text-property 0 :type cand)))
    (if (equal type :async)
        (consult-mu--update-headers query t msg :async))
    (consult-mu--reply msg )))

(defun consult-mu-embark-forward (cand)
  "Forward the message in CAND."
  (let* ((msg (get-text-property 0 :msg cand))
         (query (get-text-property 0 :query cand))
         (type (get-text-property 0 :type cand)))
    (if (equal type :async)
        (consult-mu--update-headers query t msg :async))
    (consult-mu--forward msg)))

(defun consult-mu-embark-kill-message-field (cand)
  "Get a header field of message in CAND."
  (let* ((msg (get-text-property 0 :msg cand))
         (query (get-text-property 0 :query cand))
         (type (get-text-property 0 :type cand))
         (msg-id (plist-get msg :message-id)))
    (if (equal type :async)
        (consult-mu--update-headers query t msg :async))
    (with-current-buffer consult-mu-headers-buffer-name
      (unless (equal (mu4e-message-field-at-point :message-id) msg-id)
        (mu4e-headers-goto-message-id msg-id))
      (if (equal (mu4e-message-field-at-point :message-id) msg-id)
          (progn
            (mu4e~headers-update-handler msg nil nil))))

    (with-current-buffer consult-mu-view-buffer-name
      (kill-new (consult-mu--message-get-header-field))
      (consult-mu--pulse-region (point) (line-end-position)))))

(defun consult-mu-embark-save-attachmnts (cand)
  "Save attachments of CAND."
  (let* ((msg (get-text-property 0 :msg cand))
         (query (get-text-property 0 :query cand))
         (type (get-text-property 0 :type cand))
         (msg-id (plist-get msg :message-id)))

    (if (equal type :async)
        (consult-mu--update-headers query t msg :async))

    (with-current-buffer consult-mu-headers-buffer-name
      (unless (equal (mu4e-message-field-at-point :message-id) msg-id)
        (mu4e-headers-goto-message-id msg-id))
      (if (equal (mu4e-message-field-at-point :message-id) msg-id)
          (progn
            (mu4e~headers-update-handler msg nil nil))))

    (with-current-buffer consult-mu-view-buffer-name
      (goto-char (point-min))
      (re-search-forward "^\\(Attachment\\|Attachments\\): " nil t)
      (consult-mu--pulse-region (point) (line-end-position))
      (mu4e-view-save-attachments t))))

(defun consult-mu-embark-search-messages-from-contact (cand)
  "Search messages from the same sender as the message in CAND."
  (let* ((msg (get-text-property 0 :msg cand))
         (from (car (plist-get msg :from)))
         (email (plist-get from :email)))
    (consult-mu (concat "from:" email))))

(defun consult-mu-embark-search-messages-with-subject (cand)
  "Search all messages for the same subject as the message in CAND."
  (let* ((msg (get-text-property 0 :msg cand))
         ;;(subject (replace-regexp-in-string ":\\|#\\|\\.\\|\\+" "" (plist-get msg :subject)))
         (subject (replace-regexp-in-string ":\\|#\\|\\.\\|\\+\\|\\(\\[.*\\]\\)" "" (format "%s" (plist-get msg :subject)))))
    (consult-mu (concat "subject:" subject))))

;; macro for defining functions for marks
(defmacro consult-mu-embark--defun-mark-for (mark)
  "Define a function mu4e-view-mark-for- MARK."
  (let ((funcname (intern (format "consult-mu-embark-mark-for-%s" mark)))
        (docstring (format "Mark the current message for %s." mark)))
    `(progn
       (defun ,funcname (cand) ,docstring
              (let* ((msg (get-text-property 0 :msg cand))
                     (msgid (plist-get msg  :message-id))
                     (query (get-text-property 0 :query cand))
                     (buf (get-buffer consult-mu-headers-buffer-name)))
                (if buf
                    (progn
                      (with-current-buffer buf
                        (if (eq major-mode 'mu4e-headers-mode)
                            (progn
                              (goto-char (point-min))
                              (mu4e-headers-goto-message-id msgid)
                              (if (equal (mu4e-message-field-at-point :message-id) msgid)
                                  (mu4e-headers-mark-and-next ',mark)
                                (progn
                                  (consult-mu--update-headers query t msg :async)
                                  (with-current-buffer buf
                                    (goto-char (point-min))
                                    (mu4e-headers-goto-message-id msgid)
                                    (if (equal (mu4e-message-field-at-point :message-id) msgid)
                                        (mu4e-headers-mark-and-next ',mark))))))
                          (progn
                            (consult-mu--update-headers query t msg :async)
                            (with-current-buffer buf
                              (goto-char (point-min))
                              (mu4e-headers-goto-message-id msgid)
                              (if (equal (mu4e-message-field-at-point :message-id) msgid)
                                  (mu4e-headers-mark-and-next ',mark)))))))))))))

;; add embark functions for marks
(defun consult-mu-embark--defun-func-for-marks (marks)
  "Run the macro `consult-mu-embark--defun-mark-for' on MARKS.

MARKS is a list of marks.

This is useful for creating embark functions for all the `mu4e-marks'
elements."
  (mapcar (lambda (mark) (eval `(consult-mu-embark--defun-mark-for ,mark))) marks))

;; use consult-mu-embark--defun-func-for-marks to make a function for each `mu4e-marks' element.
(consult-mu-embark--defun-func-for-marks (mapcar 'car mu4e-marks))

;;; Define Embark Keymaps
(defvar-keymap consult-mu-embark-general-actions-map
  :doc "Keymap for consult-mu-embark"
  :parent embark-general-map)

(add-to-list 'embark-keymap-alist '(consult-mu . consult-mu-embark-general-actions-map))


(defvar-keymap consult-mu-embark-messages-actions-map
  :doc "Keymap for consult-mu-embark-messages"
  :parent consult-mu-embark-general-actions-map
  "r" #'consult-mu-embark-reply
  "w" #'consult-mu-embark-wide-reply
  "f" #'consult-mu-embark-forward
  "?" #'consult-mu-embark-kill-message-field
  "c" #'consult-mu-embark-search-messages-from-contact
  "s" #'consult-mu-embark-search-messages-with-subject
  "S" #'consult-mu-embark-save-attachmnts)

(add-to-list 'embark-keymap-alist '(consult-mu-messages . consult-mu-embark-messages-actions-map))


;; add mark keys to `consult-mu-embark-messages-actions-map' keymap
(defun consult-mu-embark--add-keys-for-marks (marks)
  "Add a key for each mark in MARKS to embark map.

Adds the keys in `consult-mu-embark-messages-actions-map', and binds the
combination “m key”, where key is the :char in mark plist in the
`consult-mu-embark-messages-actions-map' to the function defined by the
prefix “consult-mu-embark-mark-for-” and mark.

This is useful for adding all `mu4e-marks' to embark key bindings under a
submenu (called by “m”), for example, the default mark-for-archive mark
that is bound to r in mu4e buffers can be called in embark by “m r”."
  (mapcar (lambda (mark)
            (let* ((key (plist-get (cdr mark) :char))
                   (key (cond ((consp key) (car key)) ((stringp key) key)))
                   (func (intern (concat "consult-mu-embark-mark-for-" (format "%s" (car mark)))))
                   (key (concat "m" key)))
              (define-key consult-mu-embark-messages-actions-map key func)))
          marks))

;; add all `mu4e-marks to embark keybindings. See `consult-mu-embark--add-keys-for-marks' above for more details
(consult-mu-embark--add-keys-for-marks mu4e-marks)

;; change the default action on `consult-mu-messages' category.
(add-to-list 'embark-default-action-overrides '(consult-mu-messages . consult-mu-embark-default-action))


;;; Provide `consult-mu-embark' module

(provide 'consult-mu-embark)

;;; consult-mu-embark.el ends here
