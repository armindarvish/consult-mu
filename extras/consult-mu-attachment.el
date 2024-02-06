;;; consult-mu-attachment.el --- Consult Mu4e asynchronously in GNU Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2023 Armin Darvish

;; Author: Armin Darvish
;; Maintainer: Armin Darvish
;; Created: 2023
;; Version: 1.0
;; Package-Requires: ((emacs "28.0") (consult "0.34"))
;; Homepage: https://github.com/armindarvish/consult-mu
;; Keywords: convenience, matching, tools, email

;;; Commentary:

;;; Code:

(require 'consult-mu)

(defun consult-mu--pulse-regexp (regexp)
  "Finds and pulses REGEXP"
  (goto-char (point-min))
  (while (re-search-forward regexp nil t)
    (when-let* ((m (match-data))
           (beg (car m))
           (end (cadr m))
           (ov (make-overlay beg end))
           (pulse-delay 0.075)
           )
      (pulse-momentary-highlight-overlay ov 'highlight))
    ))

(defun consult-mu--pulse-line ()
"Pulse line momentarily"
(let* ((pulse-delay 0.055)
      (ov (make-overlay (car (bounds-of-thing-at-point 'line)) (cdr (bounds-of-thing-at-point 'line)) )))
(pulse-momentary-highlight-overlay ov 'highlight))
)

(defun consult-mu-attach--read-file-attach (&optional initial)
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
                   (funcall preview action
                        (find-file-noselect (file-truename cand))
                        ))))
        ('return
         cand
        )
        )))
:preview-key "C-o"
))

(defun consult-mu-attach--read-file-remove (&optional file)
"Select attached files to remove from email.

FILE is the initial input in the minibuffer."
  (pcase major-mode
    ('org-msg-edit-mode
     (let* ((current-files (org-msg-get-prop "attachment"))
            (file (or file (if current-files
                               (consult--read current-files
                                              :prompt "Remove File:"
                                              :category 'file
                                              :state (lambda (action cand)
                                                       (let ((preview (consult--buffer-preview)))
                                                         (pcase action
                                                           ('preview
                                                            (if cand
                                                                (when (not (file-directory-p cand))
                                                                  (funcall preview action
                                                                           (find-file-noselect (file-truename cand))
                                                                           ))))
                                                           ('return
                                                            cand
                                                            )
                                                           )))
                                              :preview-key "C-o"
                                              )
                             (progn
                               (message "no files currently attached!")
                               nil)))))
       file))
    ('mu4e-compose-mode
     (goto-char (point-max))
     (let ((file (or file
     (consult--read
      (cl-loop while (re-search-backward "<#part.*filename=\"\\(?1:.*\\)\"[[:ascii:][:nonascii:]]*?/part>" nil t)
               collect (match-string-no-properties 1))
      :prompt "Remove File:"
      :category 'file
      :state (lambda (action cand)
               (let ((preview (consult--buffer-preview)))
                 (pcase action
                   ('preview
                    (if cand
                        (when (not (file-directory-p cand))
                          (funcall preview action
                                   (find-file-noselect (file-truename cand))
                                   ))))
                   ('return
                    cand
                    )
                   )))
      :preview-key "C-o"
      ))))
       file)
    )))

(defun consult-mu-attach--add-file (file &rest args)
  "Attach FILE to email."
  (save-excursion
    (pcase major-mode
    ('org-msg-edit-mode
  (let* ((current-files (org-msg-get-prop "attachment")))
    (if (file-directory-p file)
        (message "please select a file not a directory!")
      (if (not (member file current-files))
      (progn
        (org-msg-set-prop "attachment" (delete-dups (append current-files (list (file-truename file)))))
        (message (format "file %s added" file))
        (goto-last-change 0)
        (org-reveal)
        (consult-mu--pulse-line))
    ))))
    ('mu4e-compose-mode
             (goto-char (point-max))
             (unless (eq (current-column) 0)
               (insert "\n\n")
               (forward-line 2))
             (mail-add-attachment (file-truename file))
             (goto-last-change 0)
             (forward-line -2)
             (consult-mu--pulse-line)
             )
    )))

(defun consult-mu-attach--remove-file (file &rest args)
"Remove File from current attachments"
  (save-excursion
    (pcase major-mode
      ('org-msg-edit-mode
       (let ((current-files (org-msg-get-prop "attachment")))
         (when (member file current-files)
           (org-msg-set-prop "attachment" (remove file current-files))
           (message (format "file %s removed" file))
           (goto-last-change 0)
           (org-reveal)
           )))
      ('mu4e-compose-mode
       (goto-char (point-min))
       (when (replace-regexp (format "<#part.*filename=\"%s\"[[:ascii:][:nonascii:]]*?/part>" file) "")
         (message (format "file %s removed" file))
         (goto-last-change 0)
         (consult-mu--pulse-line)
         (whitespace-cleanup)
         ))
      )))

(defun consult-mu-attach-add (&optional file)
"Attach FILE to email interactively."
  (interactive)
  (save-mark-and-excursion
    (let* ((file (or file (consult-mu-attach--read-file-attach))))
      (while (or (null file) (file-directory-p file)) (setq file (consult-mu-attach--read-file-attach file)))
      (consult-mu-attach--add-file file)
      )))

(defun consult-mu-attach-remove (&optional file)
"Remove FILE from email attachments interactively."
  (interactive)
  (save-mark-and-excursion
      (when-let (file (consult-mu-attach--read-file-remove))
           (consult-mu-attach--remove-file file))
      ))
