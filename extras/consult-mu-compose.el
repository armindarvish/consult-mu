;;; consult-mu-compose.el --- Consult Mu4e asynchronously -*- lexical-binding: t -*-

;; Copyright (C) 2023 Armin Darvish

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

(require 'consult-mu)

;;; Customization Variables
(defcustom consult-mu-compose-use-dired-attachment 'in-dired
  "Use a Dired buffer for multiple file attachment?

If set to \='in-dired uses `dired' buffer and `dired' marks only when inside
a `dired' buffer.  If \='t, a `dired' buffer will be used for selecting attachment files similar to what Doom Emacs does:
URL `https://github.com/doomemacs/doomemacs/blob/bea81278fd2ecb65db6a63dbcd6db2f52921ee41/modules/email/mu4e/autoload/email.el#L272'.

If \='nil, consult-mu uses minibuffer completion for selection files to
attach, even if inside a `dired' buffer.

By default this is set to \='in-dired."
  :group 'consult-mu
  :type '(choice (const :tag "Only use Dired if inside Dired Buffer" 'in-dired)
                 (const :tag "Always use Dired" t)
                 (const :tag "Never use Dired" nil)))

(defcustom consult-mu-large-file-warning-threshold large-file-warning-threshold
  "Threshold for size of file to require confirmation for preview.

This is used when selecting files to attach to emails.  Files larger than this value in size will require user confirmation before previewing the file.  Default value is set by `large-file-warning-threshold'.  If nil, no cofnirmation is required."
  :group 'consult-mu
  :type '(choice integer (const :tag "Never request confirmation" nil)))


(defcustom consult-mu-compose-preview-key consult-mu-preview-key
  "Preview key for `consult-mu-compose'.

This is similar to `consult-mu-preview-key' but explicitly for
consult-mu-compose.  It is recommended to set this to something other than
\='any to avoid loading preview buffers for each file."
  :group 'consult-mu
  :type '(choice (const :tag "Any key" any)
                 (list :tag "Debounced"
                       (const :debounce)
                       (float :tag "Seconds" 0.1)
                       (const any))
                 (const :tag "No preview" nil)
                 (key :tag "Key")
                 (repeat :tag "List of keys" key)))

(defcustom consult-mu-embark-attach-file-key nil
  "Embark key binding for interactive file attachement."
  :group 'consult-mu
  :type '(choice (key :tag "Key")
                 (const :tag "no key binding" nil)))

(defvar consult-mu-compose-attach-history nil
  "History variable for file attachment.

It is used in `consult-mu-compose--read-file-attach'.")

(defvar consult-mu-compose-current-draft-buffer nil
  "Store the buffer that is being edited.")

(defun consult-mu-compose--read-file-attach (&optional initial)
  "Read files in the minibuffer to attach to an email.

INITIAL is the initial input in the minibuffer."
  (consult--read (completion-table-in-turn #'completion--embedded-envvar-table
                                           #'completion--file-name-table)
                 :prompt "Attach File: "
                 :require-match t
                 :category 'file
                 :initial (or initial default-directory)
                 :lookup (lambda (sel cands &rest args)
                           (file-truename sel))
                 :state (lambda (action cand)
                          (let ((preview (consult--buffer-preview)))
                            (pcase action
                              ('preview
                               (if cand
                                   (when (not (file-directory-p cand))
                                     (let* ((filename (file-truename cand))
                                            (filesize (float
                                                       (file-attribute-size
                                                        (file-attributes filename))))
                                            (confirm (if (and filename
                                                              (>= filesize consult-mu-large-file-warning-threshold))
                                                         (yes-or-no-p (format "File is %s Bytes.  Do you really want to preview it?" filesize))
                                                       t)))
                                       (if confirm
                                           (funcall preview action
                                                    (find-file-noselect (file-truename cand))))))))
                              ('return
                               cand))))
                 :preview-key consult-mu-compose-preview-key
                 :add-history (list mu4e-attachment-dir)
                 :history 'consult-mu-compose-attach-history))

(defun consult-mu-compose--read-file-remove (&optional initial)
  "Select attached files to remove from email.

INITIAL is the initial input in the minibuffer."

  (if-let ((current-files (pcase major-mode
                            ('org-msg-edit-mode
                             (org-msg-get-prop "attachment"))
                            ((or 'mu4e-compose-mode 'message-mode)
                             (goto-char (point-max))
                             (cl-loop while (re-search-backward "<#part.*filename=\"\\(?1:.*\\)\"[[:ascii:][:nonascii:]]*?/part>" nil t)
                                      collect (match-string-no-properties 1)))
                            (_
                             (error "Not in a compose message buffer")
                             nil))))

      (consult--read current-files
                     :prompt "Remove File:"
                     :category 'file
                     :state (lambda (action cand)
                              (let ((preview (consult--buffer-preview)))
                                (pcase action
                                  ('preview
                                   (if cand
                                       (when (not (file-directory-p cand))
                                         (let* ((filename (file-truename cand))
                                                (filesize (float
                                                           (file-attribute-size
                                                            (file-attributes filename))))
                                                (confirm (if (and filename
                                                                  (>= filesize consult-mu-large-file-warning-threshold))
                                                             (yes-or-no-p (format "File is %s Bytes.  Do you really want to preview it?" filesize))
                                                           t)))
                                           (if confirm
                                               (funcall preview action
                                                        (find-file-noselect (file-truename cand))))))))
                                  ('return
                                   cand))))
                     :preview-key consult-mu-compose-preview-key
                     :initial initial)
    (progn
      (message "No files currently attached!")
      nil)))

(defun consult-mu-compose-get-draft-buffer ()
  "Query user to select a mu4e compose draft buffer."
  (save-excursion
  (if (and (consult-mu-compose-get-current-buffers)
           (y-or-n-p "Attach the files to an existing compose buffer? "))
      (consult--read (consult-mu-compose-get-current-buffers)
                     :prompt "Select Message Buffer: "
                     :require-match nil
                     :category 'consult-mu-messages
                     :preview-key consult-mu-preview-key
                     :lookup (lambda (sel cands &rest args)
                               (or (get-buffer sel) sel))
                     :state (lambda (action cand)
                              (let ((preview (consult--buffer-preview)))
                                (pcase action
                                  ('preview
                                   (if (and cand (buffer-live-p cand))
                                       (funcall preview action
                                                cand)))
                                  ('return
                                   cand))))))))

(defun consult-mu-compose-get-current-buffers ()
  "Return a list of active compose message buffers."
  (let (buffers)
    (save-current-buffer
      (dolist (buffer (buffer-list t))
        (set-buffer buffer)
        (when (or (and (derived-mode-p 'message-mode)
                       (null message-sent-message-via))
                  (derived-mode-p 'org-msg-edit-mode)
                  (derived-mode-p 'mu4e-compose-mode))
          (push (buffer-name buffer) buffers))))
    (nreverse buffers)))

(defun consult-mu-compose--attach-files (files &optional mail-buffer &rest _args)
  "Attach FILES to email in MAIL-BUFFER compose buffer."
  (let ((files (if (stringp files) (list files) files))
        (mail-buffer (or mail-buffer (if (version<= mu4e-mu-version "1.12")
                                 (mu4e-compose 'new) (mu4e-compose-new)))))
    (with-current-buffer mail-buffer
      (pcase major-mode
        ('org-msg-edit-mode
         (save-excursion
           (let* ((new-files (delete-dups (append (org-msg-get-prop "attachment") files))))
             (org-msg-set-prop "attachment" new-files))
           (goto-last-change 0)
           (org-reveal)
           (consult-mu--pulse-line)))
        ((or 'mu4e-compose-mode 'message-mode)
         (save-excursion
           (dolist (file files)
             (goto-char (point-max))
             (unless (eq (current-column) 0)
               (insert "\n\n")
               (forward-line 2))
             (mail-add-attachment (file-truename file))
             (goto-last-change 0)
             (forward-line -2)
             (consult-mu--pulse-line))))
        (_
         (error "%s is not a compose buffer" (current-buffer)))))))

(defun consult-mu-compose--remove-files (files &optional mail-buffer &rest _args)
  "Remove FILES from current attachments in MAIL-BUFFER."
  (let ((files (if (stringp files) (list files) files))
        (mail-buffer (or mail-buffer (current-buffer))))
    (with-current-buffer mail-buffer
      (save-excursion
        (pcase major-mode
          ('org-msg-edit-mode
           (let ((current-files (org-msg-get-prop "attachment"))
                 (removed-files (list)))
             (mapcar (lambda (file)
                       (when (member file current-files)
                         (org-msg-set-prop "attachment" (delete-dups (remove file current-files)))
                         (add-to-list 'removed-files file)
                         (setq current-files (org-msg-get-prop "attachment"))
                         (goto-last-change 0)
                         (org-reveal)
                         (consult-mu--pulse-line)))
                     files)
             (message "file(s) %s detached" (mapconcat 'identity removed-files ","))))
          ('mu4e-compose-mode
           (let ((removed-files (list)))
             (mapcar (lambda (file)
                       (goto-char (point-min))
                       (while (re-search-forward (format "<#part.*filename=\"%s\"[[:ascii:][:nonascii:]]*?/part>" file) nil t)
                         (replace-match "" nil nil)
                         (setq removed-files (append removed-files (list file)))
                         (goto-last-change 0)
                         (consult-mu--pulse-line)
                         (whitespace-cleanup)))
                     files)
             (message "file(s) %s detached" (mapconcat 'identity removed-files ", ")))))))))

(defun consult-mu-compose-attach (&optional files mail-buffer)
  "Attach FILES to email in MAIL-BUFFER interactively.

MAIL-BUFFER defaults to `consult-mu-compose-current-draft-buffer'."
  (interactive)
  (let* ((consult-mu-compose-current-draft-buffer (cond
                                                   ((or (derived-mode-p 'mu4e-compose-mode) (derived-mode-p 'org-msg-edit-mode) (derived-mode-p 'message-mode)) (current-buffer))
                                                   ((derived-mode-p 'dired-mode)
                                                    (and (bound-and-true-p dired-mail-buffer) (buffer-live-p dired-mail-buffer) dired-mail-buffer))
                                                   (t
                                                    consult-mu-compose-current-draft-buffer)))
         (mail-buffer (or mail-buffer
                          (and (buffer-live-p consult-mu-compose-current-draft-buffer) consult-mu-compose-current-draft-buffer)
                          nil))
         (files (or files
                    (if (and (derived-mode-p 'dired-mode) consult-mu-compose-use-dired-attachment)
                        (delq nil
                              (mapcar
                               ;; don't attach directories
                               (lambda (f) (if (file-directory-p f)
                                               nil
                                             f))
                               (nreverse (dired-map-over-marks (dired-get-filename) nil))))
                      (consult-mu-compose--read-file-attach files)))))
    (pcase major-mode
      ((or 'mu4e-compose-mode 'org-msg-edit-mode 'message-mode)
       (setq mail-buffer (current-buffer))
       (setq consult-mu-compose-current-draft-buffer mail-buffer)
       (cond
        ((stringp files)
         (cond
          ((and (not (file-directory-p files)) (file-truename files))
           (consult-mu-compose--attach-files (file-truename files) mail-buffer))
          ((and (file-directory-p files) (eq consult-mu-compose-use-dired-attachment 'always))
           (progn
             (split-window-sensibly)
             (with-current-buffer (dired files)
               (setq-local dired-mail-buffer mail-buffer))))
          ((and (file-directory-p files) (not (eq consult-mu-compose-use-dired-attachment 'always)))
           (progn
             (while (file-directory-p files)
               (setq files (consult-mu-compose--read-file-attach files)))
             (consult-mu-compose--attach-files (file-truename files) mail-buffer)))))
        ((listp files)
         (consult-mu-compose--attach-files files mail-buffer))))
      ('dired-mode
       (setq mail-buffer (or (and (bound-and-true-p dired-mail-buffer) (buffer-live-p dired-mail-buffer) dired-mail-buffer)
                             (consult-mu-compose-get-draft-buffer)
                             (if (version<= mu4e-mu-version "1.12")
                                 (mu4e-compose 'new) (mu4e-compose-new))))

       (cond
        ((and mail-buffer (buffer-live-p mail-buffer)))
        ((stringp mail-buffer) (with-current-buffer (if (version<= mu4e-mu-version "1.12")
                                                        (mu4e-compose 'new) (mu4e-compose-new))
                                 (save-excursion (message-goto-subject)
                                                 (insert mail-buffer)
                                                 (rename-buffer mail-buffer t)))
         (setq mail-buffer (get-buffer mail-buffer))))

       (if (and mail-buffer (buffer-live-p mail-buffer))
           (progn
             (setq-local dired-mail-buffer mail-buffer)
             (switch-to-buffer mail-buffer)
             (cond
              ((not files)
               (message "no files were selected!"))
              ((stringp files)
               (cond
                ((and (file-truename files) (not (file-directory-p files)))
                 (consult-mu-compose--attach-files (file-truename files) mail-buffer))
                ((and (not consult-mu-compose-use-dired-attachment) (file-directory-p files))
                 (progn
                   (while (file-directory-p files)
                     (setq files (consult-mu-compose--read-file-attach files)))
                   (consult-mu-compose--attach-files (file-truename files) mail-buffer)))))
              ((listp files)
               (consult-mu-compose--attach-files files mail-buffer))))))
      (_
       (setq mail-buffer (or
                          consult-mu-compose-current-draft-buffer
                          (consult-mu-compose-get-draft-buffer)
                          (if (version<= mu4e-mu-version "1.12")
                              (mu4e-compose 'new) (mu4e-compose-new))))
       (cond
        ((and mail-buffer (buffer-live-p mail-buffer)))
        ((stringp mail-buffer) (with-current-buffer (if (version<= mu4e-mu-version "1.12")
                                                        (mu4e-compose 'new) (mu4e-compose-new))
                                 (save-excursion (message-goto-subject)
                                                 (insert mail-buffer)
                                                 (rename-buffer mail-buffer t)))
         (setq mail-buffer (get-buffer mail-buffer))))
       (if (and mail-buffer (buffer-live-p mail-buffer))
           (progn
             (switch-to-buffer mail-buffer)
             (setq consult-mu-compose-current-draft-buffer mail-buffer)
             (cond
              ((and (not (file-directory-p files)) (file-truename files))
               (consult-mu-compose--attach-files (file-truename files) mail-buffer))
              ((and (file-directory-p files) (eq consult-mu-compose-use-dired-attachment 'always))
               (progn
                 (split-window-sensibly)
                 (with-current-buffer (dired files)
                   (setq-local dired-mail-buffer mail-buffer)
                   )))
              ((and (file-directory-p files) (not (eq consult-mu-compose-use-dired-attachment 'always)))
               (progn
                 (while (file-directory-p files)
                   (setq files (consult-mu-compose--read-file-attach files)))
                 (consult-mu-compose--attach-files (file-truename files) mail-buffer)))
              ((listp files)
               (consult-mu-compose--attach-files files mail-buffer))))))))
  mail-buffer)

(defun consult-mu-compose-detach (&optional file)
  "Remove FILE from email attachments interactively."
  (interactive)
  (save-mark-and-excursion
    (when-let (file (consult-mu-compose--read-file-remove))
      (consult-mu-compose--remove-files file))))

;;; provide `consult-mu-compose' module
(provide 'consult-mu-compose)

;;; consult-mu-compose.el ends here
