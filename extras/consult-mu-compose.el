;;; consult-mu-compose.el --- Consult Mu4e asynchronously in GNU Emacs -*- lexical-binding: t -*-

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

;;; Customization Variables
(defcustom consult-mu-compose-use-dired-attachment nil
  "Use a dired buffer for multiple file attachment?
By defualt this is set to nil.
"
  :group 'consult-mu
  :type 'boolean)

(defcustom consult-mu-compose-preview-key consult-mu-preview-key
  "Preview key for `consult-mu-compose'.

This is similar `consult-mu-preview-key' but explicitly for consult-mu-compose."
  :type '(choice (const :tag "Any key" any)
                 (list :tag "Debounced"
                       (const :debounce)
                       (float :tag "Seconds" 0.1)
                       (const any))
                 (const :tag "No preview" nil)
                 (key :tag "Key")
                 (repeat :tag "List of keys" key)))

(defvar consult-mu-compose-attach-history nil
  "History variable for file attachment used in `consult-mu-compose--read-file-attach'.")

(defvar consult-mu-compose-current-draft-buffer nil
  "Stores the buffer that is being edited.")

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

;; (defun consult-mu-compose--read-file-attach (&optional initial)
;;   "Read files in the minibuffer to attach to an email.

;; INITIAL is the initial input in the minibuffer."
;;   (consult--read (completion-table-in-turn #'completion--embedded-envvar-table
;;                                            #'completion--file-name-table)
;;                  :prompt "Attach File: "
;;                  :require-match t
;;                  :category 'file
;;                  :initial (or initial default-directory)
;;                  :lookup (lambda (sel cands &rest args)
;;                            (file-truename sel))
;;                  :state (lambda (action cand)
;;                           (let ((preview (consult--buffer-preview)))
;;                             (pcase action
;;                               ('setup
;;                                (setq consult-mu-compose-current-draft-buffer (current-buffer))
;;                                )
;;                               ('preview
;;                                (if cand
;;                                    (when (not (file-directory-p cand))
;;                                      (funcall preview action
;;                                               (find-file-noselect (file-truename cand))
;;                                               ))))
;;                               ('return
;;                                cand
;;                                )
;;                               )))
;;                  :preview-key consult-mu-compose-preview-key
;;                  :history 'consult-mu-compose-attach-history
;;                  ))

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
                                     (funcall preview action
                                              (find-file-noselect (file-truename cand))
                                              ))))
                              ('return
                               cand
                               )
                              )))
                 :preview-key consult-mu-compose-preview-key
                 :history 'consult-mu-compose-attach-history
                 ))

(defun consult-mu-compose--read-file-remove (&optional file)
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
                                              :preview-key consult-mu-compose-preview-key
                                              )
                             (progn
                               (message "no files currently attached!")
                               nil)))))
       file))
    ((or 'mu4e-compose-mode 'message-mode)
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
      :preview-key consult-mu-compose-preview-key
      ))))
       file)
    )))

(defun consult-mu-compose-get-draft-buffer ()
  "Queries user to select a mu4e draft buffer"
  (save-excursion
  (if (and (consult-mu-compose-get-current-buffers)
           (y-or-n-p "Attach files to existing mail composition buffer? "))
      (consult--read (consult-mu-compose-get-current-buffers)
                     :prompt "Message: "
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
                                                cand)
                                     ))
                                  ('return
                                   cand
                                   )
                                  )))
                     ))))

(defun consult-mu-compose-get-current-buffers ()
  "Return a list of active message buffers."
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

;; (defun consult-mu-compose--attach-file (file &rest args)
;;   "Attach FILE to email."
;;   (pcase major-mode
;;     ('org-msg-edit-mode
;;      (save-excursion
;;        (let* ((current-files (org-msg-get-prop "attachment")))
;;          (if (file-directory-p file)
;;              (message "please select a file not a directory!")
;;            (if (not (member file current-files))
;;                (progn
;;                  (org-msg-set-prop "attachment" (delete-dups (append current-files (list (file-truename file)))))
;;                  (message (format "file %s added" file))
;;                  (goto-last-change 0)
;;                  (org-reveal)
;;                  (consult-mu--pulse-line))
;;              )))))
;;     ('mu4e-compose-mode
;;      (save-excursion
;;        (goto-char (point-max))
;;        (unless (eq (current-column) 0)
;;          (insert "\n\n")
;;          (forward-line 2))
;;        (mail-add-attachment (file-truename file))
;;        (goto-last-change 0)
;;        (forward-line -2)
;;        (consult-mu--pulse-line)
;;        ))
;;     ))

(defun consult-mu-compose--attach-files (files &optional mail-buffer &rest args)
  "Attach FILE to email in MAIL-BUFFER."
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
           (consult-mu--pulse-line))
         )
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
             (consult-mu--pulse-line)
             )))
        ))))

;; (defun consult-mu-compose--read-file-attach (&optional initial)
;;   "Read files in the minibuffer to attach to an email.

;; INITIAL is the initial input in the minibuffer."
;;   (consult--read (completion-table-in-turn #'completion--embedded-envvar-table
;;                                            #'completion--file-name-table)
;;                  :prompt "Attach File: "
;;                  :require-match t
;;                  :category 'file
;;                  :initial (or initial default-directory)
;;                  :lookup (lambda (sel cands &rest args)
;;                            (file-truename sel))
;;                  :state (lambda (action cand)
;;                           (let ((preview (consult--buffer-preview)))
;;                             (pcase action
;;                               ('setup
;;                                ;;(setq consult-mu-compose-current-draft-buffer (current-buffer))
;;                                )
;;                               ('preview
;;                                ;;(print consult-mu-compose-current-draft-buffer)
;;                                (if cand
;;                                    (when (not (file-directory-p cand))
;;                                      (funcall preview action
;;                                               (find-file-noselect (file-truename cand))
;;                                               ))))
;;                               ('return
;;                                cand
;;                                )
;;                               )))
;;                  :preview-key consult-mu-compose-preview-key
;;                  :history 'consult-mu-compose-attach-history
;;                  ))

;; (defun consult-mu-compose-dired-attach-files (&optional files-to-attach)
;;   "Attaches files to mu4e message using a dired buffer.
;; When called in a mail compose buffer, `read-file-name'to either
;; attach a file, or select a folder to open dired in.
;; When called in a dired buffer, attaches marked files to a message. If a compose buffer exists,
;; asks user whether to attach to existing message otherwise opens a new message darft buffer and attaches the files.
;; When called in a mu4e-compose or org-msg buffer,  and select file attachments
;; (using `consult-mu-compose-dired-attach-mode').

;; When otherwise called, open a dired buffer and enable `consult-mu-compose-dired-attach-mode'."
;;   ;; TODO add ability to attach files (+dirs) as a single (named) archive
;;   (interactive "p")
;;   (pcase major-mode

;;     ((or 'mu4e-compose-mode 'org-msg-edit-mode)
;;      (let ((mail-buffer (current-buffer))
;;            (location (consult-mu-compose--read-file-attach)))
;;        (if (and (file-truename location) (not (file-directory-p location)))
;;            (consult-mu-compose--attach-file (file-truename location))
;;          (if (and (file-directory-p location) consult-mu-compose-use-dired-attachment)
;;            (progn
;;              (split-window-sensibly)
;;              (with-current-buffer (dired location)
;;                (setq-local dired-mail-buffer mail-buffer)
;;                ))
;;            (if (file-directory-p location)
;;                (progn
;;                  (while (file-directory-p location)
;;                    (setq location (consult-mu-compose--read-file-attach location)))
;;                  (consult-mu-compose--attach-file (file-truename location)))))))
;;      )
;;     ('dired-mode
;;      (unless (and files-to-attach (/= 1 files-to-attach))
;;        (setq files-to-attach
;;              (delq nil
;;                    (mapcar
;;                     ;; don't attach directories
;;                     (lambda (f) (if (file-directory-p f)
;;                                     nil
;;                                   f))
;;                           (nreverse (dired-map-over-marks (dired-get-filename) nil))))))
;;      (if (not files-to-attach)
;;          (message "No files marked to attach!")
;;        (if-let ((mail-target-buffer (bound-and-true-p dired-mail-buffer)))
;;           (switch-to-buffer mail-target-buffer)
;;          (if (and (consult-mu-compose-get-current-buffers)
;;                   (y-or-n-p "Attach files to existing mail composition buffer? "))
;;              (progn (setf mail-target-buffer
;;                           (completing-read "Message: " (consult-mu-compose-get-current-buffers)))
;;                     (if (one-window-p) (kill-buffer)
;;                     (kill-buffer-and-window))
;;                     (switch-to-buffer mail-target-buffer))
;;            (progn (if (one-window-p) (kill-buffer)
;;                     (kill-buffer-and-window))
;;                   (mu4e-compose 'new)))))
;;      (mapcar
;;       (pcase major-mode
;;         ('mu4e-compose-mode #'mail-add-attachment)
;;         ('org-msg-edit-mode #'org-msg-attach-attach))
;;       files-to-attach)
;;      (save-excursion
;;        (goto-last-change 0)
;;        (pcase major-mode
;;          ('org-msg-edit-mode (org-reveal))
;;          ('mu4e-compose-mode (forward-line -2)))
;;        (consult-mu--pulse-line))
;;      )
;;     (_
;;      (let ((location (consult-mu-compose--read-file-attach)))
;;        (when (file-directory-p location)
;;          (if consult-mu-compose-use-dired-attachment
;;            (progn
;;              (split-window-sensibly)
;;              (dired (file-truename location))
;;               )
;;            (while (file-directory-p location)
;;              (setq location (consult-mu-compose--read-file-attach location)))))
;;          (if (and (consult-mu-compose-get-current-buffers)
;;                     (y-or-n-p "Attach files to existing mail composition buffer? "))
;;                (let ((mail-buffer
;;                       (completing-read "Message: " (consult-mu-compose-get-current-buffers))))
;;                  (switch-to-buffer mail-buffer))
;;              (mu4e-compose 'new))
;;            (consult-mu-compose--attach-file (file-truename location)))
;;          ))
;;     )

(defun consult-mu-compose--remove-file (file &rest args)
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

(defun consult-mu-compose-attach (&optional files mail-buffer)
  "Attach FILES to email interactively."
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
                     (consult-mu-compose--read-file-attach files))))
         )
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
               (setq-local dired-mail-buffer mail-buffer)
               )))
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
                                 (mu4e-compose 'new) (mu4e-compose-new))
                             ))

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
               (consult-mu-compose--attach-files files mail-buffer)))
             )))
      (_
       (setq mail-buffer (or
                          consult-mu-compose-current-draft-buffer
                          (consult-mu-compose-get-draft-buffer)
                             (if (version<= mu4e-mu-version "1.12")
                                 (mu4e-compose 'new) (mu4e-compose-new))
                             ))
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
                 (consult-mu-compose--attach-files (file-truename files) mail-buffer)
                 )
               )
              ((listp files)
               (consult-mu-compose--attach-files files mail-buffer))))))))
  mail-buffer)

(defun consult-mu-compose-detach (&optional file)
"Remove FILE from email attachments interactively."
  (interactive)
  (save-mark-and-excursion
      (when-let (file (consult-mu-compose--read-file-remove))
           (consult-mu-compose--remove-file file)
           )
      ))

;;; provide `consult-mu-compose' module
(provide 'consult-mu-compose)

;;;  consult-mu-compose.el ends here
