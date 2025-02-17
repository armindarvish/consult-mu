;;; consult-mu.el --- Consult Mu4e asynchronously -*- lexical-binding: t -*-

;; Copyright (C) 2023 Armin Darvish

;; Author: Armin Darvish
;; Maintainer: Armin Darvish
;; Created: 2023
;; Version: 1.0
;; Package-Requires: ((emacs "28.0") (consult "2.0"))
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
(require 'consult)
(require 'mu4e)

;;; Group

(defgroup consult-mu nil
  "Options for `consult-mu'."
  :group 'convenience
  :group 'minibuffer
  :group 'consult
  :group 'mu4e
  :prefix "consult-mu-")

;;; Customization Variables

(defcustom consult-mu-args '("mu")
  "Command line arguments to call `mu` asynchronously.

The dynamically computed arguments are appended.
Can be either a string, or a list of strings or expressions."
  :group 'consult-mu
  :type '(choice string (repeat (choice string sexp))))

(defcustom consult-mu-maxnum mu4e-search-results-limit
  "Maximum number of results.

This is normally passed to “--maxnum” in the command line or is defined by
`mu4e-search-results-limit'.  By default inherits from
`mu4e-search-results-limit'."
  :group 'consult-mu
  :type '(choice (const :tag "Unlimited" -1)
                 (integer :tag "Limit")))

(defcustom consult-mu-search-sort-field mu4e-search-sort-field
  "What field to sort results by?

By defualt inherits from `mu4e-search-sort-field'."
  :group 'consult-mu
  :type '(radio (const :tag "Date" :date)
                (const :tag "Subject" :subject)
                (const :tag "File Size" :size)
                (const :tag "Priority" :prio)
                (const :tag "From (Sender)" :from)
                (const :tag "To (Recipients)" :to)
                (const :tag "Mailing List" :list)))

(defcustom consult-mu-headers-fields mu4e-headers-fields
  "A list of header fields to show in the headers buffer.

By default inherits from `mu4e-headers-field'.

From mu4e docs:

Each element has the form (HEADER . WIDTH), where HEADER is one of
the available headers (see `mu4e-header-info') and WIDTH is the
respective width in characters.

A width of nil means “unrestricted”, and this is best reserved
for the rightmost \(last\) field.  Note that Emacs may become very
slow with excessively long lines \(1000s of characters\), so if you
regularly get such messages, you want to avoid fields with nil
altogether."
  :group 'consult-mu
  :type `(repeat (cons (choice ,@(mapcar (lambda (h)
                                           (list 'const
                                                 :tag (plist-get (cdr h) :help)
                                                 (car h)))
                                         mu4e-header-info))
                       (choice (integer :tag "width")
                               (const :tag "unrestricted width" nil)))))

(defcustom consult-mu-headers-template nil
  "A template string to make custom header formats.

If non-nil, `consult-mu' uses this string to format the headers instead of
`consult-mu-headers-field'.

The string should be of the format “%[char][integer]%[char][integer]...”,
and allow dynamic insertion of the content.  Each “%[char][integer]“ chunk
represents a different field and the integer defines the length of the
field.

The list of available fields are:

  %f  sender(s) \(e.g. from: field of email\)
  %t  receivers(s) \(i.e. to: field of email\)
  %s  subject \(i.e. title of email\)
  %d  date \(i.e. the date email was sent/received\)
  %p  priority
  %z  size
  %i  message-id \(as defined by mu\)
  %g  flags \(as defined by mu\)
  %G  pretty flags \(this uses `mu4e~headers-flags-str' to pretify flags\)
  %x  tags \(as defined by mu\)
  %c  cc \(i.e. cc: field of the email\)
  %h  bcc \(i.e. bcc: field of the email\)
  %r  date chaged \(as defined by :changed in mu4e\)

For exmaple, “%d15%s50” means 15 characters for date and 50 charcters for
subject, and “%d13%s37%f17” would make a header containing 13 characters
for Date, 37 characters for Subject, and 20 characters for From field,
making a header that looks like this:

Thu 09 Nov 23  Title of the Email Limited to 50 Char...  example@domain..."
  :group 'consult-mu
  :type '(choice (const :tag "Fromatted String" :format "%{%%d13%%s50%%f17%}")
                 (function :tag "Custom Function")))

(defcustom consult-mu-search-sort-direction mu4e-search-sort-direction
  "Direction to sort by a symbol.

By defualt inherits from `mu4e-search-sort-direction', and can either be
\='descending (sorting  Z->A) or \='ascending (sorting A->Z)."

  :group 'consult-mu
  :type '(radio (const ascending)
                (const descending)))


(defcustom consult-mu-search-threads mu4e-search-threads
  "Whether to calculate threads for search results.

By defualt inherits from `mu4e-search-threads'.

Note that per mu4e docs:
When threading is enabled, the headers are exclusively sorted
chronologically (:date) by the newest message in the thread."
  :group 'consult-mu
  :type 'boolean)

(defcustom consult-mu-group-by nil
  "What field to use to group the results in the minibuffer.

By default it is set to :date, but can be any of:

  :subject      group by subject
  :from         group by the name/email the sender(s)
  :to           group by name/email of the reciver(s)
  :date         group by date
  :time         group by the time of email \(i.e. hour, minute, seconds\)
  :datetime     group by date and time of the email
  :year         group by the year of the email \(i.e. 2023, 2022, ...\)
  :month        group by the month of the email \(i.e. Jan, Feb, ..., Dec\)
  :week         group by the week number of the email
                \(i.e. 1st week, 2nd week, ... 52nd week\)
  :day-of-week  group by the day email was sent (i.e. Mondays, Tuesdays, ...)
  :day          group by the day email was sent (similar to :day-of-week)
  :size         group by the file size of the email
  :flags        group by flags (as defined by mu)
  :tags         group by tags (as defined by mu)
  :changed      group by the date changed
                \(as defined by :changed field in mu4e\)"
  :group 'consult-mu
  :type '(radio (const :date)
                (const :subject)
                (const :from)
                (const :to)
                (const :time)
                (const :datetime)
                (const :year)
                (const :month)
                (const :week)
                (const :day-of-week)
                (const :day)
                (const :size)
                (const :flags)
                (const :tags)
                (const :changed)
                (const nil)))

(defcustom consult-mu-mark-previewed-as-read nil
  "Whether to mark PREVIEWED emails as read or not?"
  :group 'consult-mu
  :type 'boolean)

(defcustom consult-mu-mark-viewed-as-read t
  "Whether to mark VIEWED emails as read or not?"
  :group 'consult-mu
  :type 'boolean)

(defcustom consult-mu-headers-buffer-name "*consult-mu-headers*"
  "Default name for HEADERS buffer explicitly for `consult-mu'.

For more info see `mu4e-headers-buffer-name'."
  :group 'consult-mu
  :type 'string)

(defcustom consult-mu-view-buffer-name "*consult-mu-view*"
  "Default name for VIEW buffer explicitly for `consult-mu'.

For more info see `mu4e-view-buffer-name'."
  :group 'consult-mu
  :type 'string)

(defcustom consult-mu-preview-key consult-preview-key
  "Preview key for `consult-mu'.

This is similar to `consult-preview-key' but explicitly for `consult-mu'."
  :group 'consult-mu
  :type '(choice (symbol :tag "Any key" 'any)
                 (list :tag "Debounced"
                       (const :debounce)
                       (float :tag "Seconds" 0.1)
                       (const any))
                 (const :tag "No preview" nil)
                 (key :tag "Key")
                 (repeat :tag "List of keys" key)))


(defcustom consult-mu-highlight-matches t
  "Should `consult-mu' highlight search queries in preview buffers?"
  :group 'consult-mu
  :type 'boolean)

(defcustom consult-mu-use-wide-reply 'ask
  "Reply to all or not?

This defines whether `consult-mu--reply-action' should reply to all or not."
  :group 'consult-mu
  :type '(choice (symbol :tag "Ask for confirmation" 'ask)
                 (const :tag "Do not reply to all" nil)
                 (const :tag "Always reply to all" t)))

(defcustom consult-mu-action #'consult-mu--view-action
  "The function that is used when selecting a message.
By default it is bound to `consult-mu--view-action'."
  :group 'consult-mu
  :type '(choice (function :tag "(Default) View Message in Mu4e Buffers" consult-mu--view-action)
                 (function :tag "Reply to Message" consult-mu--reply-action)
                 (function :tag "Forward Message" consult-mu--forward-action)
                 (function :tag "Custom Function")))

(defcustom consult-mu-default-command #'consult-mu-dynamic
  "Which command should `consult-mu' call."
  :group 'consult-mu
  :type '(choice (function :tag "(Default) Use Dynamic Collection (i.e. `consult-mu-dynamic')" #'consult-mu-dynamic)
                 (function :tag "Use Async Collection (i.e. `consult-mu-async')"  #'consult-mu-async)
                 (function :tag "Custom Function")))

;;; Other Variables
(defvar consult-mu-category 'consult-mu
  "Category symbol for the `consult-mu' package.")

(defvar consult-mu-messages-category 'consult-mu-messages
  "Category symbol for messages in `consult-mu' package.")

(defvar consult-mu--view-buffers-list (list)
  "List of currently open preview buffers for `consult-mu'.")

(defvar consult-mu--history nil
  "History variable for `consult-mu'.")

(defvar consult-mu-delimiter "      "
  "Delimiter to use for fields in mu command output.

The idea is Taken from  https://github.com/seanfarley/counsel-mu.")

(defvar consult-mu-saved-searches-dynamic (list)
  "List of Favorite searches for `consult-mu-dynamic'.")

(defvar consult-mu-saved-searches-async consult-mu-saved-searches-dynamic
  "List of Favorite searches for `consult-mu-async'.")

(defvar consult-mu--override-group nil
  "Override grouping in `consult-mu' based on user input.")

(defvar consult-mu--mail-headers '("Subject" "From" "To" "From/To" "Cc" "Bcc" "Reply-To" "Date" "Attachments" "Tags" "Flags" "Maildir" "Summary" "List" "Path" "Size" "Message-Id" "List-Id" "Changed")
  "List of possible headers in a message.")

;;; Faces

(defface consult-mu-highlight-match-face
  `((t :inherit 'consult-highlight-match))
  "Highlight match face in `consult-mu' view buffer.

By default inherits from `consult-highlight-match'.
This is used to highlight matches of search queries in the minibufffer
completion list.")

(defface consult-mu-preview-match-face
  `((t :inherit 'consult-preview-match))
  "Preview match face in `consult-mu' preview buffers.

By default inherits from `consult-preview-match'.
This is used to highlight matches of search query terms in preview buffers
\(i.e. `consult-mu-view-buffer-name'\).")

(defface consult-mu-default-face
  `((t :inherit 'default))
  "Default face in `consult-mu' minibuffer annotations.

By default inherits from `default' face.")

(defface consult-mu-subject-face
  `((t :inherit 'font-lock-keyword-face))
  "Subject face in `consult-mu' minibuffer annotations.

By default inherits from `font-lock-keyword-face'.")

(defface consult-mu-sender-face
  `((t :inherit 'font-lock-variable-name-face))
  "Contact face in `consult-mu' minibuffer annotations.

By default inherits from `font-lock-variable-name-face'.")

(defface consult-mu-receiver-face
  `((t :inherit 'font-lock-variable-name-face))
  "Contact face in `consult-mu' minibuffer annotations.

By default inherits from `font-lock-variable-name-face'.")

(defface consult-mu-date-face
  `((t :inherit 'font-lock-preprocessor-face))
  "Date face in `consult-mu' minibuffer annotations.

By default inherits from `font-lock-preprocessor-face'.")

(defface consult-mu-count-face
  `((t :inherit 'font-lock-string-face))
  "Count face in `consult-mu' minibuffer annotations.

By default inherits from `font-lock-string-face'.")

(defface consult-mu-size-face
  `((t :inherit 'font-lock-string-face))
  "Size face in `consult-mu' minibuffer annotations.

By default inherits from `font-lock-string-face'.")

(defface consult-mu-tags-face
  `((t :inherit 'font-lock-comment-face))
  "Tags/Comments face in `consult-mu' minibuffer annotations.

By default inherits from `font-lock-comment-face'.")

(defface consult-mu-flags-face
  `((t :inherit 'font-lock-function-call-face))
  "Flags face in `consult-mu' minibuffer annotations.

By default inherits from `font-lock-function-call-face'.")

(defface consult-mu-url-face
  `((t :inherit 'link))
  "URL face in `consult-mu' minibuffer annotations;

By default inherits from `link'.")

(defun consult-mu--pulse-regexp (regexp)
  "Find and pulse REGEXP."
  (goto-char (point-min))
  (while (re-search-forward regexp nil t)
    (when-let* ((m (match-data))
                (beg (car m))
                (end (cadr m))
                (ov (make-overlay beg end))
                (pulse-delay 0.075))
      (pulse-momentary-highlight-overlay ov 'highlight))))

(defun consult-mu--pulse-region (beg end)
  "Find and pulse region from BEG to END."
  (let ((ov (make-overlay beg end))
        (pulse-delay 0.075))
    (pulse-momentary-highlight-overlay ov 'highlight)))

(defun consult-mu--pulse-line ()
  "Pulse line at point momentarily."
  (let* ((pulse-delay 0.055)
         (ov (make-overlay (car (bounds-of-thing-at-point 'line))
                           (cdr (bounds-of-thing-at-point 'line)))))
    (pulse-momentary-highlight-overlay ov 'highlight)))

(defun consult-mu--set-string-width (string width &optional prepend)
  "Set the STRING width to a fixed value, WIDTH.

If the STRING is longer than WIDTH, it truncates the string and adds
ellipsis, “...”.  If the string is shorter, it adds whitespace to the
string.  If PREPEND is non-nil, it truncates or adds whitespace from the
beginning of string, instead of the end."
  (let* ((string (format "%s" string))
         (w (string-width string)))
    (when (< w width)
      (if prepend
          (setq string (format "%s%s" (make-string (- width w) ?\s) (substring string)))
        (setq string (format "%s%s" (substring string) (make-string (- width w) ?\s)))))
    (when (> w width)
      (if prepend
          (setq string (format "...%s" (substring string (- w (- width 3)) w)))
        (setq string (format "%s..." (substring string 0 (- width (+ w 3)))))))
    string))

(defun consult-mu--justify-left (string prefix maxwidth)
  "Set the width of  STRING+PREFIX justified from left.

Use `consult-mu--set-string-width' to the width of the concatenate of
STRING+PREFIX \(e.g. “(concat prefix string)”\) within MAXWIDTH.  This is
used for aligning marginalia info in the minibuffer."
  (let ((w (string-width prefix)))
    (if (> maxwidth w)
        (consult-mu--set-string-width string (- maxwidth w) t)
      string)))

(defun consult-mu--highlight-match (regexp str ignore-case)
  "Highlight REGEXP in STR.

If a REGEXP contains a capturing group, only the captured group is
highlighted, otherwise, the whole match is highlighted.
Case is ignored if IGNORE-CASE is non-nil.
\(This is adapted from `consult--highlight-regexps'.\)"
  (let ((i 0))
    (while (and (let ((case-fold-search ignore-case))
                  (string-match regexp str i))
                (> (match-end 0) i))
      (let ((m (match-data)))
        (setq i (cadr m)
              m (or (cddr m) m))
        (while m
          (when (car m)
            (add-face-text-property (car m) (cadr m)
                                    'consult-mu-highlight-match-face nil str))
          (setq m (cddr m))))))
  str)

(defun consult-mu--overlay-match (match-str buffer ignore-case)
  "Highlight MATCH-STR in BUFFER using an overlay.

If IGNORE-CASE is non-nil, it uses case-insensitive match.

This is used to highlight matches to use queries when viewing emails.  See
`consult-mu-overlays-toggle' for toggling highligths on/off."
  (with-current-buffer (or (get-buffer buffer) (current-buffer))
    (remove-overlays (point-min) (point-max) 'consult-mu-overlay t)
    (goto-char (point-min))
    (let ((case-fold-search ignore-case))
      (while (search-forward match-str nil t)
        (when-let* ((m (match-data))
                    (beg (car m))
                    (end (cadr m))
                    (overlay (make-overlay beg end)))
          (overlay-put overlay 'consult-mu-overlay t)
          (overlay-put overlay 'face 'consult-mu-highlight-match-face))))))

(defun consult-mu-overlays-toggle (&optional buffer)
  "Toggle overlay highlight in BUFFER.

BUFFER defaults to `current-buffer'."
  (interactive)
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (dolist (o (overlays-in (point-min) (point-max)))
        (when (overlay-get o 'consult-mu-overlay)
          (if (and (overlay-get o 'face) (eq (overlay-get o 'face) 'consult-mu-highlight-match-face))
              (overlay-put o 'face nil)
            (overlay-put o 'face 'consult-mu-highlight-match-face)))))))

(defun consult-mu--format-date (string)
  "Format the date STRING from mu output.

STRING is the output form a mu command, for example:
`mu find query --fields d`
Returns the date in the format Day-of-Week Month Day Year Time
\(e.g. Sat Nov 04 2023 09:46:54\)"
  (let ((string (replace-regexp-in-string " " "0" string)))
    (format "%s %s %s"
            (substring string 0 10)
            (substring string -4 nil)
            (substring string 11 -4))))

(defun consult-mu-flags-to-string (FLAG)
  "Covert FLAGS, from mu output to strings.

FLAG is the output form mu command in the terminal, for example:
 `mu find query --fields g`.
This function converts each character in FLAG to an expanded string of the
flag and returns the list of these strings."
  (cl-loop for c across FLAG
           collect
           (pcase (string c)
             ("D" 'draft)
             ("F" 'flagged)
             ("N" 'new)
             ("P" 'forwarded)
             ("R" 'replied)
             ("S" 'read)
             ("T" 'trashed)
             ("a" 'attachment)
             ("x" 'encrrypted)
             ("s" 'signed)
             ("u" 'unread)
             ("l" 'list)
             ("q" 'personal)
             ("c" 'calendar)
             (_ nil))))

(defun consult-mu--message-extract-email-from-string (string)
  "Find and return the first email address in the STRING."
  (when (stringp string)
    (string-match "[a-zA-Z0-9\_\.\+\-]+@[a-zA-Z0-9\-]+\.[a-zA-Z0-9\-\.]+" string)
    (match-string 0 string)))

(defun consult-mu--message-emails-string-to-list (string)
  "Convert comma-separated STRING of email addresses to a list."
  (when (stringp string)
    (remove '(" " "\s" "\t")
            (mapcar #'consult-mu--message-extract-email-from-string
                    (split-string string ",\\|;\\|\t" t)))))

(defun consult-mu--message-get-header-field (&optional field)
  "Retrive FIELD header from the message/mail in the current buffer."
  (save-match-data
    (save-excursion
      (when (or (derived-mode-p 'message-mode)
                (derived-mode-p 'mu4e-view-mode)
                (derived-mode-p 'org-msg-edit-mode)
                (derived-mode-p 'mu4e-compose-mode))
        (let* ((case-fold-search t)
               (header-regexp (mapconcat (lambda (str) (concat "\n" str ": "))
                                        consult-mu--mail-headers "\\|"))
               (field (or (downcase field)
                          (downcase (consult--read consult-mu--mail-headers
                                                   :prompt "Header Field: ")))))
          (if (string-prefix-p "attachment" field) (setq field "\\(attachment\\|attachments\\)"))
          (goto-char (point-min))
          (message-goto-body)
          (let* ((match (re-search-backward (concat "^" field ": \\(?1:[[:ascii:][:nonascii:]]*?\\)\n\\(.*?:\\|\n\\)") nil t))
                 (str (if (and match (match-string 1)) (string-trim (match-string 1)))))
            (if (string-empty-p str) nil str)))))))

(defun consult-mu--headers-append-handler (msglst)
  "Append one-line descriptions of messages in MSGLST.

This is used to override `mu4e~headers-append-handler' to ensure that
buffer handling is done right for `consult-mu'."
  (with-current-buffer "*consult-mu-headers*"
    (let ((inhibit-read-only t))
      (seq-do
       ;; I use mu4e-column-faces and it overrides the default append-handler. To get the same effect I check if mu4e-column-faces is active and enabled.
       (if (and (featurep 'mu4e-column-faces) mu4e-column-faces-mode)
           (lambda (msg)
             (mu4e-column-faces--insert-header msg (point-max)))
         (lambda (msg)
           (mu4e~headers-insert-header msg (point-max))))
       msglst))))

(defun consult-mu--view-msg (msg &optional buffername)
  "Display the message MSG in a buffer with BUFFERNAME.

BUFFERNAME defaults to `consult-mu-view-buffer-name'.

This s used to overrides `mu4e-view' to ensure that buffer handling is done
right for `consult-mu'."
  (let* ((linked-headers-buffer (mu4e-get-headers-buffer "*consult-mu-headers*" t))
         (mu4e-view-buffer-name (or buffername consult-mu-view-buffer-name)))
    (setq gnus-article-buffer (mu4e-get-view-buffer linked-headers-buffer t))
    (with-current-buffer gnus-article-buffer
      (let ((inhibit-read-only t))
        (remove-overlays (point-min) (point-max) 'mu4e-overlay t)
        (erase-buffer)
        (insert-file-contents-literally
         (mu4e-message-readable-path msg) nil nil nil t)
        (setq-local mu4e--view-message msg)
        (mu4e--view-render-buffer msg)
        (mu4e-loading-mode 0)
        (with-current-buffer linked-headers-buffer
          (setq-local mu4e~headers-view-win (mu4e-display-buffer gnus-article-buffer nil)))
        (run-hooks 'mu4e-view-rendered-hook)))))

(defun consult-mu--headers-clear (&optional text)
  "Clear the headers buffer and related data structures.

Optionally, show TEXT.

This is used to override `mu4e~headers-clear' to ensure that buffer
handling is done right for `consult-mu'."
  (setq mu4e~headers-render-start (float-time)
        mu4e~headers-hidden 0)
  (with-current-buffer "*consult-mu-headers*"
    (let ((inhibit-read-only t))
      (mu4e--mark-clear)
      (erase-buffer)
      (when text
        (goto-char (point-min))
        (insert (propertize text 'face 'mu4e-system-face 'intangible t))))))

(defun consult-mu--set-mu4e-search-sortfield (opts)
  "Dynamically set the `mu4e-search-sort-field' based on user input.

Uses user input (i.e. from `consult-mu' command) to define the sort field.

OPTS is the command line options for mu and can be set by entering options
in the minibuffer input.  For more details, refer to `consult-grep' and
consult async documentation.

For example if the user enters the following in the minibuffer:

“#query -- --maxnum 400 --sortfield from”

`mu4e-search-sort-field' is set to :from

Note that per mu4e docs:
When threading is enabled, the headers are exclusively sorted
chronologically (:date) by the newest message in the thread."
  (let* ((sortfield (cond
                     ((member "-s" opts) (nth (+ (cl-position "-s" opts :test 'equal) 1) opts))
                     ((member "--sortfield" opts) (nth (+ (cl-position "--sortfield" opts :test 'equal) 1) opts))
                     (t consult-mu-search-sort-field))))
    (pcase sortfield
      ('nil
       consult-mu-search-sort-field)
      ((or "date" "d")
       :date)
      ((or "subject" "s")
       :subject)
      ((or "size" "z")
       :size)
      ((or "prio" "p")
       :prio)
      ((or "from" "f")
       :from)
      ((or "to" "t")
       :to)
      ((or "list" "v")
       :list)
      ;; ((or "tags" "x")
      ;;  :tags)
      (_
       consult-mu-search-sort-field))))

(defun consult-mu--set-mu4e-search-sort-direction (opts)
  "Dynamically set the `mu4e-search-sort-direction' based on user input.

Uses user input \(i.e. from `consult-mu' command\) to define the sort field.

OPTS is the command line options for mu and can be set by entering options
in the minibuffer input.  For more details, refer to `consult-grep' and
consult async documentation.

For example, if the user enters the following in the minibuffer:

“#query -- --maxnum 400 --sortfield from --reverse”

The `mu4e-search-sort-direction' is reversed; If it is set to
\='ascending, it is toggled to \='descending and vise versa."
  (if (or (member "-z" opts) (member "--reverse" opts))
      (pcase consult-mu-search-sort-direction
        ('descending
         'ascending)
        ('ascending
         'descending))
    consult-mu-search-sort-direction))

(defun consult-mu--set-mu4e-skip-duplicates (opts)
  "Dynamically set the `mu4e-search-skip-duplicates' based on user input.

Uses user input \(i.e. from `consult-mu' command\) to define whether to
skip duplicates.

OPTS is the command line options for mu and can be set by entering options
in the minibuffer input.  For more details, refer to `consult-grep' and
consult async documentation.

For example, if the user enters the following in the minibuffer:

“#query -- --maxnum 400 --skip-dups”

The `mu4e-search-skip-duplicates' is set to t."
  (if (or (member "--skip-dups" opts) mu4e-search-skip-duplicates) t nil))

(defun consult-mu--set-mu4e-results-limit (opts)
  "Dynamically set the `mu4e-search-results-limit' based on user input.


Uses user input \(i.e. from `consult-mu' command\) to define the number of
results shown.

OPTS is the command line options for mu and can be set by entering options
in the minibuffer input.  For more details, refer to `consult-grep' and
consult async documentation.

For example, if the user enters the following in the minibuffer:

“#query -- --maxnum 400”

The `mu4e-search-results-limit' is set to 400."
  (cond
   ((member "-n" opts) (string-to-number (nth (+ (cl-position "-n" opts :test 'equal) 1) opts)))
   ((member "--maxnum" opts) (string-to-number (nth (+ (cl-position "--maxnum" opts :test 'equal) 1) opts)))
   (t consult-mu-maxnum)))


(defun consult-mu--set-mu4e-include-related (opts)
  "Dynamically set the `mu4e-search-include-related' based on user input.

Uses user input \(i.e. from `consult-mu' command\) to define whether to
include related messages.

OPTS is the command line options for mu and can be set by entering options
in the minibuffer input.  For more details, refer to `consult-grep' and
consult async documentation.

For example if the user enters the following in the minibuffer:

“#query -- --include-related”

The `mu4e-search-include-related' is set to t."
  (if (or (member "-r" opts) (member "--include-related" opts) mu4e-search-include-related) t nil))



(defun consult-mu--set-mu4e-threads (opts)
  "Set  the `mu4e-search-threads' based on `mu4e-search-sort-field'.

Uses user input \(i.e. from `consult-mu' command\) to define whether to
show threads.

OPTS is the command line options for mu and can be set by entering options
in the minibuffer input.  For more details, refer to `consult-grep' and
consult async documentation.

Note that per mu4e docs, when threading is enabled, the headers are
exclusively sorted by date.  Here the logic is reversed in order to allow
dynamically sorting by fields other than date \(even when threads are
enabled\).  In other words, if the sort-field is not the :date, threading
is disabled because otherwise sort field will be ignored.  This allows the
user to use command line arguments to sort messages by fields other than
the date.  For example, the user can enter the following in the minibuffer
input to sort by subject

“#query -- --sortfield subject”

When the sort-field is :date, the default setting,
`consult-mu-search-threads' is used, and if that is set to nil, the user
can use command line arguments \(a.k.a. -t or --thread\) to enable it
dynamically."
  (cond
   ((not (equal mu4e-search-sort-field :date))
    nil)
   ((or (member "-t" opts) (member "--threads" opts) consult-mu-search-threads)
    t)))

(defun consult-mu--update-headers (query ignore-history msg type)
  "Search for QUERY, and update `consult-mu-headers-buffer-name' buffer.

If IGNORE-HISTORY is true, does *not* update the query history stack,
`mu4e--search-query-past'.
If MSG is non-nil, put the cursor on MSG.
TYPE can be either \=':dynamic or \=':async"
  (consult-mu--execute-all-marks)
  (cl-letf* (((symbol-function #'mu4e~headers-append-handler) #'consult-mu--headers-append-handler))
    (unless (mu4e-running-p) (mu4e--server-start))
    (let* ((buf (mu4e-get-headers-buffer consult-mu-headers-buffer-name t))
           (view-buffer (get-buffer consult-mu-view-buffer-name))
           (expr (car (consult--command-split (substring-no-properties query))))
           (rewritten-expr (funcall mu4e-query-rewrite-function expr))
           (mu4e-headers-fields consult-mu-headers-fields))
      (pcase type
        (:dynamic)
        (:async
         (setq rewritten-expr (funcall mu4e-query-rewrite-function (concat "msgid:" (plist-get msg :message-id)))))
        (_ ))

      (with-current-buffer buf
        (save-excursion
          (let ((inhibit-read-only t))
            (erase-buffer)
            (mu4e-headers-mode)
            (setq-local mu4e-view-buffer-name consult-mu-view-buffer-name)
            (if view-buffer
                (setq-local mu4e~headers-view-win (mu4e-display-buffer gnus-article-buffer nil)))
            (unless ignore-history
                                        ; save the old present query to the history list
              (when mu4e--search-last-query
                (mu4e--search-push-query mu4e--search-last-query 'past)))
            (setq mu4e--search-last-query rewritten-expr)
            (setq list-buffers-directory rewritten-expr)
            (mu4e--modeline-update)
            (run-hook-with-args 'mu4e-search-hook expr)
            (consult-mu--headers-clear mu4e~search-message)
            (setq mu4e~headers-search-start (float-time))

            (pcase-let* ((`(,_arg . ,opts) (consult--command-split query))
                         (mu4e-search-sort-field (consult-mu--set-mu4e-search-sortfield opts))
                         (mu4e-search-sort-direction (consult-mu--set-mu4e-search-sort-direction opts))
                         (mu4e-search-skip-duplicates (consult-mu--set-mu4e-skip-duplicates opts))
                         (mu4e-search-results-limit (consult-mu--set-mu4e-results-limit opts))
                         (mu4e-search-threads (consult-mu--set-mu4e-threads opts))
                         (mu4e-search-include-related (consult-mu--set-mu4e-include-related opts)))
              (mu4e--server-find
               rewritten-expr
               mu4e-search-threads
               mu4e-search-sort-field
               mu4e-search-sort-direction
               mu4e-search-results-limit
               mu4e-search-skip-duplicates
               mu4e-search-include-related))
            (while (or (string-empty-p (buffer-substring (point-min) (point-max)))
                       (equal (buffer-substring (point-min) (+ (point-min) (length mu4e~search-message))) mu4e~search-message)
                       (not (or (equal (buffer-substring (- (point-max) (length mu4e~no-matches)) (point-max)) mu4e~no-matches) (equal (buffer-substring (- (point-max) (length mu4e~end-of-results)) (point-max)) mu4e~end-of-results))))
              (sleep-for 0.005))))))))

(defun consult-mu--execute-all-marks (&optional no-confirmation)
  "Execute the actions for all marked messages.

Executes all actions for marked messages in the buffer
`consult-mu-headers-buffer-name'.

If NO-CONFIRMATION is non-nil, don't ask user for confirmation.

This is similar to `mu4e-mark-execute-all' but, with buffer/window
handling set accordingly for `consult-mu'."
  (interactive "P")
  (when-let* ((buf (get-buffer consult-mu-headers-buffer-name)))
    (with-current-buffer buf
      (when (eq major-mode 'mu4e-headers-mode)
        (mu4e--mark-in-context
         (let* ((marknum (mu4e-mark-marks-num)))
           (unless (zerop marknum)
             (pop-to-buffer buf)
             (unless (one-window-p) (delete-other-windows))
             (mu4e-mark-execute-all no-confirmation)
             (quit-window))))))))

(defun consult-mu--headers-goto-message-id (msgid)
  "Jump to message with MSGID.

This is done in `consult-mu-headers-buffer-name' buffer."
  (when-let ((buffer consult-mu-headers-buffer-name))
    (with-current-buffer buffer
      (setq mu4e-view-buffer-name consult-mu-view-buffer-name)
      (mu4e-headers-goto-message-id msgid))))

(defun consult-mu--get-message-by-id (msgid)
  "Find the message with MSGID and return the mu4e MSG plist for it."
  (cl-letf* (((symbol-function #'mu4e-view) #'consult-mu--view-msg))
    (when-let ((buffer consult-mu-headers-buffer-name))
      (with-current-buffer buffer
        (setq mu4e-view-buffer-name consult-mu-view-buffer-name)
        (mu4e-headers-goto-message-id msgid)
        (mu4e-message-at-point)))))

(defun consult-mu--contact-string-to-plist (string)
  "Convert STRING for contacts to plist.

STRING is the output form mu command, for example from:
`mu find query --fields f`

Returns a plist with \=':email and \':name keys.

For example

“John Doe <john.doe@example.com>”

will be converted to

\(:name “John Doe” :email “john.doe@example.com”\)"
  (let* ((string (replace-regexp-in-string ">,\s\\|>;\s" ">\n" string))
         (list (split-string string "\n" t)))
    (mapcar (lambda (item)
              (cond
               ((string-match "\\(?2:.*\\)\s+<\\(?1:.+\\)>" item)
                (list :email (or (match-string 1 item) nil) :name (or (match-string 2 item) nil)))
               ((string-match "^\\(?1:[a-zA-Z0-9\_\.\+\-]+@[a-zA-Z0-9\-]+\.[a-zA-Z0-9\-\.]+\\)" item)
                (list :email (or (match-string 1 item) nil) :name nil))
               (t
                (list :email (format "%s" item) :name nil)))) list)))

(defun consult-mu--contact-name-or-email (contact)
  "Retrieve name or email of CONTACT.

Looks at the contact plist \(e.g. (:name “John Doe” :email
“john.doe@example.com”)\) and returns the name.  If the name is missing,
returns the email address."
  (cond
   ((stringp contact)
    contact)
   ((listp contact)
    (mapconcat (lambda (item) (or (plist-get item :name) (plist-get item :email) "")) contact ","))))

(defun consult-mu--headers-template ()
  "Make headers template using `consult-mu-headers-template'."
  (if (and consult-mu-headers-template (functionp consult-mu-headers-template))
      (funcall consult-mu-headers-template)
    consult-mu-headers-template))

(defun consult-mu--expand-headers-template (msg string)
  "Expand STRING to create a custom header format for MSG.

See `consult-mu-headers-template' for explanation of the format of
STRING."

  (cl-loop for c in (split-string string "%" t)
           concat (concat (pcase  (substring c 0 1)
                            ("f" (let ((sender (consult-mu--contact-name-or-email (plist-get msg :from)))
                                       (length (string-to-number (substring c 1 nil))))
                                   (if sender
                                       (propertize (if (> length 0) (consult-mu--set-string-width sender length) sender) 'face 'consult-mu-sender-face))))
                            ("t" (let ((receiver (consult-mu--contact-name-or-email (plist-get msg :to)))
                                       (length (string-to-number (substring c 1 nil))))
                                   (if receiver
                                       (propertize (if (> length 0) (consult-mu--set-string-width receiver length) receiver) 'face 'consult-mu-sender-face))))
                            ("s" (let ((subject (plist-get msg :subject))
                                       (length (string-to-number (substring c 1 nil))))
                                   (if subject
                                       (propertize (if (> length 0) (consult-mu--set-string-width subject length) subject) 'face 'consult-mu-subject-face))))
                            ("d" (let ((date (format-time-string "%a %d %b %y" (plist-get msg :date)))
                                       (length (string-to-number (substring c 1 nil))))
                                   (if date
                                       (propertize (if (> length 0) (consult-mu--set-string-width date length) date) 'face 'consult-mu-date-face))))

                            ("p" (let ((priority (plist-get msg :priority))
                                       (length (string-to-number (substring c 1 nil))))
                                   (if priority
                                       (propertize (if (> length 0) (consult-mu--set-string-width (format "%s" priority) length) (format "%s" priority)) 'face 'consult-mu-size-face))))
                            ("z" (let ((size (file-size-human-readable (plist-get msg :size)))
                                       (length (string-to-number (substring c 1 nil))))
                                   (if size
                                       (propertize (if (> length 0) (consult-mu--set-string-width size length) size)  'face 'consult-mu-size-face))))
                            ("i" (let ((id (plist-get msg :message-id))
                                       (length (string-to-number (substring c 1 nil))))
                                   (if id
                                       (propertize (if (> length 0) (consult-mu--set-string-width id length) id) 'face 'consult-mu-default-face))))

                            ("g" (let ((flags  (plist-get msg :flags))
                                       (length (string-to-number (substring c 1 nil))))
                                   (if flags
                                       (propertize (if (> length 0) (consult-mu--set-string-width (format "%s" flags) length) (format "%s" flags)) 'face 'consult-mu-flags-face))))

                            ("G" (let ((flags (plist-get msg :flags))
                                       (length (string-to-number (substring c 1 nil))))
                                   (if flags
                                       (propertize (if (> length 0) (consult-mu--set-string-width (format "%s" (mu4e~headers-flags-str flags)) length) (format "%s" (mu4e~headers-flags-str flags))) 'face 'consult-mu-flags-face))))

                            ("x" (let ((tags (plist-get msg :tags))
                                       (length (string-to-number (substring c 1 nil))))
                                   (if tags
                                       (propertize (if (> length 0) (consult-mu--set-string-width tags length) tags) 'face 'consult-mu-tags-face) nil)))

                            ("c" (let ((cc (consult-mu--contact-name-or-email (plist-get msg :cc)))
                                       (length (string-to-number (substring c 1 nil))))
                                   (if cc
                                       (propertize (if (> length 0) (consult-mu--set-string-width cc length) cc) 'face 'consult-mu-tags-face))))

                            ("h" (let ((bcc (consult-mu--contact-name-or-email (plist-get msg :bcc)))
                                       (length (string-to-number (substring c 1 nil))))
                                   (if bcc
                                       (propertize (if (> length 0) (consult-mu--set-string-width bcc length) bcc) 'face 'consult-mu-tags-face))))

                            ("r" (let ((changed (format-time-string "%a %d %b %y" (plist-get msg :changed)))
                                       (length (string-to-number (substring c 1 nil))))
                                   (if changed
                                       (propertize (if (> length 0) (consult-mu--set-string-width changed length) changed) 'face 'consult-mu-tags-face))))
                            (_ nil))
                          "  ")))

(defun consult-mu--quit-header-buffer ()
  "Quits `consult-mu-headers-buffer-name' buffer."
  (save-mark-and-excursion
    (when-let* ((buf (get-buffer consult-mu-headers-buffer-name)))
      (with-current-buffer buf
        (if (eq major-mode 'mu4e-headers-mode)
            (mu4e-mark-handle-when-leaving)
          (quit-window t)
          ;; clear the decks before going to the main-view
          (mu4e--query-items-refresh 'reset-baseline))))))

(defun consult-mu--quit-view-buffer ()
  "Quits `consult-mu-view-buffer-name' buffer."
  (when-let* ((buf (get-buffer consult-mu-view-buffer-name)))
    (with-current-buffer buf
      (if (eq major-mode 'mu4e-view-mode)
          (mu4e-view-quit)))))

(defun consult-mu--quit-main-buffer ()
  "Quits `mu4e-main-buffer-name' buffer."
  (when-let* ((buf (get-buffer mu4e-main-buffer-name)))
    (with-current-buffer buf
      (if (eq major-mode 'mu4e-main-mode)
          (mu4e-quit)))))

(defun consult-mu--lookup ()
  "Lookup function for `consult-mu' or `consult-mu-async' candidates.

This is passed as LOOKUP to `consult--read' on candidates and is used to
format the output when a candidate is selected."
  (lambda (sel cands &rest _args)
    (let* ((info (cdr (assoc sel cands)))
           (msg  (plist-get info :msg))
           (subject (plist-get msg :subject)))
      (cons subject info))))

(defun consult-mu--group-name (cand)
  "Get the group name of CAND using `consult-mu-group-by'.

See `consult-mu-group-by' for details of grouping options."
  (let* ((msg (get-text-property 0 :msg cand))
         (group (or consult-mu--override-group consult-mu-group-by))
         (field (if (not (keywordp group)) (intern (concat ":" (format "%s" group))) group)))
    (pcase field
      (:date (format-time-string "%a %d %b %y" (plist-get msg field)))
      (:from (cond
              ((listp (plist-get msg field))
               (mapconcat (lambda (item) (or (plist-get item :name) (plist-get item :email))) (plist-get msg field) ";"))
              ((stringp (plist-get msg field)) (plist-get msg field))))
      (:to (cond
            ((listp (plist-get msg field))
             (mapconcat (lambda (item) (or (plist-get item :name) (plist-get item :email))) (plist-get msg field) ";"))
            ((stringp (plist-get msg field)) (plist-get msg field))))
      (:changed (format-time-string "%a %d %b %y" (plist-get msg field)))
      (:datetime (format-time-string "%F %r" (plist-get msg :date)))
      (:time (format-time-string "%X" (plist-get msg :date)))
      (:year (format-time-string "%Y" (plist-get msg :date)))
      (:month (format-time-string "%B" (plist-get msg :date)))
      (:day-of-week (format-time-string "%A" (plist-get msg :date)))
      (:day (format-time-string "%A" (plist-get msg :date)))
      (:week (format-time-string "%V" (plist-get msg :date)))
      (:size (file-size-human-readable (plist-get msg field)))
      (:flags (format "%s" (plist-get msg field)))
      (:tags (format "%s" (plist-get msg field)))
      (_ (if (plist-get msg field) (format "%s" (plist-get msg field)) nil)))))

(defun consult-mu--group (cand transform)
  "Group function for `consult-mu' or `consult-mu-async'.

CAND is passed to `consult-mu--group-name' to get the group for CAND.
When TRANSFORM is non-nil, the name of CAND is used for group."
  (when-let ((name (consult-mu--group-name cand)))
    (if transform (substring cand) name)))

(defun consult-mu--view (msg noselect mark-as-read match-str)
  "Opens MSG in `consult-mu-headers' and `consult-mu-view'.

If NOSELECT is non-nil, does not select the view buffer/window.
If MARK-AS-READ is non-nil, marks the MSG as read.
If MATCH-STR is non-nil, highlights the MATCH-STR in the view buffer."
  (let ((msgid (plist-get msg :message-id)))
    (when-let ((buf (mu4e-get-headers-buffer consult-mu-headers-buffer-name t)))
      (with-current-buffer buf
        ;;(mu4e-headers-mode)
        (goto-char (point-min))
        (setq mu4e-view-buffer-name consult-mu-view-buffer-name)
        (unless noselect
          (switch-to-buffer buf))))

    (consult-mu--view-msg msg consult-mu-view-buffer-name)

    (with-current-buffer consult-mu-headers-buffer-name
      (if msgid
          (progn
            (mu4e-headers-goto-message-id msgid)
            (if mark-as-read
                (mu4e--server-move (mu4e-message-field-at-point :docid) nil "+S-u-N")))))

    (when match-str
      (add-to-history 'search-ring match-str)
      (consult-mu--overlay-match match-str consult-mu-view-buffer-name t))

    (with-current-buffer consult-mu-view-buffer-name
      (goto-char (point-min)))

    (unless noselect
      (when msg
        (select-window (get-buffer-window consult-mu-view-buffer-name))))
    consult-mu-view-buffer-name))


(defun consult-mu--view-action (cand)
  "Open the candidate, CAND.

This is a wrapper function around `consult-mu--view'.  It parses CAND to
extract relevant MSG plist and other information and passes them to
`consult-mu--view'.

To use this as the default action for `consult-mu', set
`consult-mu-default-action' to \=#'consult-mu--view-action."

  (let* ((info (cdr cand))
         (msg (plist-get info :msg))
         (query (plist-get info :query))
         (match-str (car (consult--command-split query))))
    (consult-mu--view msg nil consult-mu-mark-viewed-as-read match-str)
    (consult-mu-overlays-toggle consult-mu-view-buffer-name)))

(defun consult-mu--reply (msg &optional wide-reply)
  "Reply to MSG using `mu4e-compose-reply'.

If WIDE-REPLY is non-nil use wide-reply \(a.k.a. reply all\) with
`mu4e-compose-wide-reply'."
  (let ((msgid (plist-get msg :message-id)))
    (when-let ((buf (mu4e-get-headers-buffer consult-mu-headers-buffer-name t)))
      (with-current-buffer buf
        (goto-char (point-min))
        (setq mu4e-view-buffer-name consult-mu-view-buffer-name)))


    (with-current-buffer consult-mu-headers-buffer-name
      (mu4e-headers-goto-message-id msgid)
      (if (not wide-reply)
          (mu4e-compose-reply)
        (mu4e-compose-wide-reply)))))

(defun consult-mu--reply-action (cand &optional wide-reply)
  "Reply to CAND.

This is a wrapper function around `consult-mu--reply'.  It passes
relevant message plist, from CAND, as well as WIDE-REPLY to
`consult-mu--reply'.

To use this as the default action for `consult-mu', set
`consult-mu-default-action' to \=#'consult-mu--reply-action."
  (let* ((info (cdr cand))
         (msg (plist-get info :msg))
         (wide-reply (or wide-reply
                         (pcase consult-mu-use-wide-reply
                           ('ask (y-or-n-p "Reply All?"))
                           ('nil nil)
                           ('t t)))))
    (consult-mu--reply msg wide-reply)))

(defun consult-mu--forward (msg)
  "Forward the MSG using `mu4e-compose-forward'."
  (let ((msgid (plist-get msg :message-id)))
    (when-let ((buf (mu4e-get-headers-buffer consult-mu-headers-buffer-name t)))
      (with-current-buffer buf
        (goto-char (point-min))
        (setq mu4e-view-buffer-name consult-mu-view-buffer-name)))
    (with-current-buffer consult-mu-headers-buffer-name
      (mu4e-headers-goto-message-id msgid)
      (mu4e-compose-forward))))

(defun consult-mu--forward-action (cand)
  "Forward CAND.

This is a wrapper function around `consult-mu--forward'.  It passes
the relevant message plist, from CAND to `consult-mu--forward'.

To use this as the default action for `consult-mu', set
`consult-mu-default-action' to \=#'consult-mu--forward-action."
  (let* ((info (cdr cand))
         (msg (plist-get info :msg)))
    (consult-mu--forward msg)))

(defun consult-mu--get-split-style-character (&optional style)
  "Get the character for consult async split STYLE.

STYLE defaults to `consult-async-split-style'."
  (let ((style (or style consult-async-split-style 'none)))
    (or (char-to-string (plist-get (alist-get style consult-async-split-styles-alist) :initial))
        (char-to-string (plist-get (alist-get style consult-async-split-styles-alist) :separator))
        "")))

(defun consult-mu--dynamic-format-candidate (cand highlight)
  "Format minibuffer candidate, CAND.

CAND is the minibuffer completion candidate \(a mu4e message collected by
`consult-mu--dynamic-collection'\).  If HIGHLIGHT is non-nil, it is
highlighted with `consult-mu-highlight-match-face'."

  (let* ((string (car cand))
         (info (cadr cand))
         (msg (plist-get info :msg))
         (query (plist-get info :query))
         (match-str (if (stringp query) (consult--split-escaped (car (consult--command-split query))) nil))
         (headers-template (consult-mu--headers-template))
         (str (if headers-template
                  (consult-mu--expand-headers-template msg headers-template)
                string))
         (str (propertize str :msg msg :query query :type :dynamic)))
    (if (and consult-mu-highlight-matches highlight)
        (cond
         ((listp match-str)
          (mapc (lambda (match) (setq str (consult-mu--highlight-match match str t))) match-str))
         ((stringp match-str)
          (setq str (consult-mu--highlight-match match-str str t))))
      str)
    (when msg
      (cons str (list :msg msg :query query :type :dynamic)))))

(defun consult-mu--dynamic-collection (input)
  "Dynamically collect mu4e search results.

INPUT is the user input.  It is passed as QUERY to
`consult-mu--update-headers', appends the result to
`consult-mu-headers-buffer-name' and returns a list of found
messages."

  (save-excursion
    (pcase-let* ((`(,_arg . ,opts) (consult--command-split input)))
      (consult-mu--update-headers (substring-no-properties input) nil nil :dynamic)
      (if (or (member "-g" opts)  (member "--group" opts))
          (cond
           ((member "-g" opts)
            (setq consult-mu--override-group (intern (or (nth (+ (cl-position "-g" opts :test 'equal) 1) opts) "nil"))))
           ((member "--group" opts)
            (setq consult-mu--override-group (intern (or (nth (+ (cl-position "--group" opts :test 'equal) 1) opts) "nil")))))
        (setq consult-mu--override-group nil)))

    (with-current-buffer consult-mu-headers-buffer-name
      (goto-char (point-min))
      (remove nil
              (cl-loop until (eobp)
                       collect (consult-mu--dynamic-format-candidate (list (buffer-substring (point) (line-end-position)) (list :msg (ignore-errors (mu4e-message-at-point)) :query input)) t)
                       do (forward-line 1))))))

(defun consult-mu--dynamic-state ()
  "State function for `consult-mu' candidates.
This is passed as STATE to `consult--read' and is used to preview or do
other actions on the candidate."
  (lambda (action cand)
    (let ((preview (consult--buffer-preview)))
      (pcase action
        ('preview
         (if cand
             (when-let* ((info (cdr cand))
                         (msg (plist-get info :msg))
                         (query (plist-get info :query))
                         (msgid (substring-no-properties (plist-get msg :message-id)))
                         (match-str (car (consult--command-split query)))
                         (match-str (car (consult--command-split query)))
                         (mu4e-headers-buffer-name consult-mu-headers-buffer-name)
                         (buffer consult-mu-view-buffer-name))
               ;;(get-buffer-create consult-mu-view-buffer-name)
               (add-to-list 'consult-mu--view-buffers-list buffer)
               (funcall preview action
                        (consult-mu--view msg t consult-mu-mark-previewed-as-read match-str))
               (with-current-buffer consult-mu-view-buffer-name
                 (unless (one-window-p) (delete-other-windows))))))
        ('return
         (save-mark-and-excursion
           (consult-mu--execute-all-marks))
         (setq consult-mu--override-group nil)
         cand)))))

(defun consult-mu--dynamic (prompt collection &optional initial)
  "Query mu4e messages dyunamically.

This is a non-interactive internal function.  For the interactive version
see `consult-mu'.

It runs the `consult-mu--dynamic-collection' to do a `mu4e-search' with
user input \(e.g. INITIAL\) and returns the results \(list of messages
found\) as a completion table in minibuffer.

The completion table gets dynamically updated as the user types in the
minibuffer.  Each candidate in the minibuffer is formatted by
`consult-mu--dynamic-format-candidate' to add annotation and other info to
the candidate.

Description of Arguments:
  PROMPT     the prompt in the minibuffer
             \(passed as PROMPT to   `consult--read'\)
  COLLECTION a colection function passed to `consult--dynamic-collection'.
  INITIAL    an optional arg for the initial input in the minibuffer.
             \(passed as INITITAL to `consult--read'\)

commandline arguments/options \(see `mu find --help` in the command line
for details\) can be passed to the minibuffer input similar to
`consult-grep'.  For example the user can enter:

“#paper -- --maxnum 200 --sortfield from --reverse”

this will search for mu4e messages with the query “paper”, retrives a
maximum of 200 messages and sorts them by the “from:” field and reverses
the sort direction (opposite of `consult-mu-search-sort-field').

Note that some command line arguments are not supported by mu4e (for
example sorting based on cc: or bcc: fields are not supported in
`mu4e-search-sort-field')

Also, the results can further be narrowed by
`consult-async-split-style' \(e.g. by entering “#” when
`consult-async-split-style' is set to \='perl\).

For example:

“#paper -- --maxnum 200 --sortfield from --reverse#accepted”

will retrieve the message as the example above, then narrows down the
candidates to those that  that match “accepted”."
  (consult--read
   (consult--dynamic-collection (or collection #'consult-mu--dynamic-collection))
   :prompt (or prompt "Select: ")
   :lookup (consult-mu--lookup)
   :state (funcall #'consult-mu--dynamic-state)
   :initial initial
   :group #'consult-mu--group
   :add-history (append (list (thing-at-point 'symbol))
                        consult-mu-saved-searches-dynamic)
   :history '(:input consult-mu--history)
   :require-match t
   :category 'consult-mu-messages
   :preview-key consult-mu-preview-key
   :sort nil))

(defun consult-mu-dynamic (&optional initial noaction)
  "Lists results of `mu4e-search' dynamically.

This is an interactive wrapper function around `consult-mu--dynamic'.  It
queries the user for a search term in the minibuffer, then fetches a list
of messages for the entered search term as a minibuffer completion table
for selection.  The list of candidates in the completion table are
dynamically updated as the user changes the entry.

Upon selection of a candidate either
 - the candidate is returned if NOACTION is non-nil
 or
 - the candidate is passed to `consult-mu-action' if NOACTION is nil.

Additional commandline arguments can be passed in the minibuffer entry by
typing “--” followed by command line arguments.

For example, the user can enter:

“#consult-mu -- -n 10”

this will run a `mu4e-search' with the query “consult-mu” and changes the
search limit \(i.e. `mu4e-search-results-limit' to 10\).


Also, the results can further be narrowed by
`consult-async-split-style' \(e.g. by entering “#” when
`consult-async-split-style' is set to \='perl\).

For example:

“#consult-mu -- -n 10#github”

will retrieve the messages as the example above, then narrows down the
completion table to candidates that match “github”.

INITIAL is an optional arg for the initial input in the minibuffer.
\(passed as INITITAL to `consult-mu--dynamic'\)

For more details on consult--async functionalities, see `consult-grep' and
the official manual of consult, here:
URL `https://github.com/minad/consult'"
  (interactive)
  (save-mark-and-excursion
    (consult-mu--execute-all-marks))
  (let* ((sel
          (consult-mu--dynamic (concat "[" (propertize "consult-mu-dynamic" 'face 'consult-mu-sender-face) "]" " Search For:  ") #'consult-mu--dynamic-collection initial)))
    (save-mark-and-excursion
      (consult-mu--execute-all-marks))
    (if noaction
        sel
      (progn
        (funcall consult-mu-action sel)
        sel))))

(defun consult-mu--async-format-candidate (string input highlight)
  "Formats minibuffer candidates for `consult-mu-async'.

STRING is the output retrieved from `mu find INPUT ...` in the command line.
INPUT is the query from the user.

If HIGHLIGHT is t, input is highlighted with
`consult-mu-highlight-match-face' in the minibuffer."

  (let* ((query input)
         (parts (split-string (replace-regexp-in-string "^\\\\->\s\\|^\\\/->\s" "" string) consult-mu-delimiter))
         (msgid (car parts))
         (date (date-to-time (cadr parts)))
         (sender (cadr (cdr parts)))
         (sender (consult-mu--contact-string-to-plist sender))
         (receiver (cadr (cdr (cdr parts))))
         (receiver (consult-mu--contact-string-to-plist receiver))
         (subject (cadr (cdr (cdr (cdr parts)))))
         (size (string-to-number (cadr (cdr (cdr (cdr (cdr parts)))))))
         (flags (consult-mu-flags-to-string (cadr (cdr (cdr (cdr (cdr (cdr parts))))))))
         (tags (cadr (cdr (cdr (cdr (cdr (cdr (cdr parts))))))))
         (priority (cadr (cdr (cdr (cdr (cdr (cdr (cdr (cdr parts)))))))))
         (cc (cadr (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr parts))))))))))
         (cc (consult-mu--contact-string-to-plist cc))
         (bcc (cadr (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr parts)))))))))))
         (bcc (consult-mu--contact-string-to-plist bcc))
         (path (cadr (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr parts))))))))))))
         (msg (list :subject subject :date date :from sender :to receiver :size size :message-id msgid :flags flags :tags tags :priority priority :cc cc :bcc bcc :path path))
         (match-str (if (stringp input) (consult--split-escaped (car (consult--command-split query))) nil))
         (headers-template (consult-mu--headers-template))
         (str (if headers-template
                  (consult-mu--expand-headers-template msg headers-template)
                (format "%s\s\s%s\s\s%s\s\s%s\s\s%s\s\s%s"
                        (propertize (consult-mu--set-string-width
                                     (format-time-string "%x" date) 10)
                                    'face 'consult-mu-date-face)
                        (propertize (consult-mu--set-string-width (consult-mu--contact-name-or-email sender) (floor (* (frame-width) 0.2)))  'face 'consult-mu-sender-face)
                        (propertize (consult-mu--set-string-width subject (floor (* (frame-width) 0.55))) 'face 'consult-mu-subject-face)
                        (propertize (file-size-human-readable size) 'face 'consult-mu-size-face)
                        (propertize (format "%s" flags) 'face 'consult-mu-flags-face)
                        (propertize (if tags (format "%s" tags) nil) 'face 'consult-mu-tags-face))))
         (str (propertize str :msg msg :query query :type :async)))
    (if (and consult-mu-highlight-matches highlight)
        (cond
         ((listp match-str)
          (mapc (lambda (match) (setq str (consult-mu--highlight-match match str t))) match-str))
         ((stringp match-str)
          (setq str (consult-mu--highlight-match match-str str t))))
      str)
    (cons str (list :msg msg :query query :type :async))))

(defun consult-mu--async-state ()
  "State function for `consult-mu-async' candidates.

This is passed as STATE to `consult--read' and is used to preview or do
other actions on the candidate."
  (lambda (action cand)
    (let ((preview (consult--buffer-preview)))
      (pcase action
        ('preview
         (if cand
             (when-let* ((info (cdr cand))
                         (msg (plist-get info :msg))
                         (msgid (substring-no-properties (plist-get msg :message-id)))
                         (query (plist-get info :query))
                         (match-str (car (consult--command-split query)))
                         (mu4e-headers-buffer-name consult-mu-headers-buffer-name)
                         (buffer consult-mu-view-buffer-name))
               (add-to-list 'consult-mu--view-buffers-list buffer)
               (funcall preview action
                        (consult-mu--view msg t consult-mu-mark-previewed-as-read match-str))
               (with-current-buffer consult-mu-view-buffer-name
                 (unless (one-window-p) (delete-other-windows))))))
        ('return
         (save-mark-and-excursion
           (consult-mu--execute-all-marks))
         cand)))))

(defun consult-mu--async-transform (input)
  "Add annotation to minibuffer candiates for `consult-mu'.

Format each candidates with `consult-gh--repo-format' and INPUT."
  (lambda (cands)
    (cl-loop for cand in cands
             collect
             (consult-mu--async-format-candidate cand input t))))

(defun consult-mu--async-builder (input)
  "Build mu command line for searching messages by INPUT (e.g. `mu find INPUT)`."
  (pcase-let* ((consult-mu-args (append consult-mu-args '("find")))
               (cmd (consult--build-args consult-mu-args))
               (`(,arg . ,opts) (consult--command-split input))
               (flags (append cmd opts))
               (sortfield (cond
                           ((member "-s" flags) (nth (+ (cl-position "-s" opts :test 'equal) 1) flags))
                           ((member "--sortfield" flags) (nth (+ (cl-position "--sortfield" flags :test 'equal) 1) flags))
                           (t (substring (symbol-name consult-mu-search-sort-field) 1))))
               (threads (if (not (equal sortfield :date)) nil (or (member "-t" flags) (member "--threads" flags) mu4e-search-threads)))
               (skip-dups (or (member "-u" flags) (member "--skip-dups" flags) mu4e-search-skip-duplicates))
               (include-related (or (member "-r" flags) (member "--include-related" flags) mu4e-search-include-related)))
    (if (or (member "-g" flags)  (member "--group" flags))
        (cond
         ((member "-g" flags)
          (setq consult-mu--override-group (intern (or (nth (+ (cl-position "-g" opts :test 'equal) 1) opts) "nil")))
          (setq opts (remove "-g" (remove (nth (+ (cl-position "-g" opts :test 'equal) 1) opts) opts))))
         ((member "--group" flags)
          (setq consult-mu--override-group (intern (or (nth (+ (cl-position "--group" opts :test 'equal) 1) opts) "nil")))
          (setq opts (remove "--group" (remove (nth (+ (cl-position "--group" opts :test 'equal) 1) opts) opts)))))
      (setq consult-mu--override-group nil))
    (setq opts (append opts (list "--nocolor")))
    (setq opts (append opts (list "--fields" (format "i%sd%sf%st%ss%sz%sg%sx%sp%sc%sh%sl"
                                                     consult-mu-delimiter consult-mu-delimiter consult-mu-delimiter consult-mu-delimiter consult-mu-delimiter consult-mu-delimiter consult-mu-delimiter consult-mu-delimiter consult-mu-delimiter consult-mu-delimiter consult-mu-delimiter))))
    (unless (or (member "-s" flags) (member "--sortfiled" flags))
      (setq opts (append opts (list "--sortfield" (substring (symbol-name consult-mu-search-sort-field) 1)))))
    (if threads (setq opts (append opts (list "--thread"))))
    (if skip-dups (setq opts (append opts (list "--skip-dups"))))
    (if include-related (setq opts (append opts (list "--include-related"))))
    (cond
     ((and (member "-n" flags) (< (string-to-number (nth (+ (cl-position "-n" opts :test 'equal) 1) opts)) 0))
      (setq opts (remove "-n" (remove (nth (+ (cl-position "-n" opts :test 'equal) 1) opts) opts))))
     ((and (member "--maxnum" flags) (< (string-to-number (nth (+ (cl-position "--maxnum" opts :test 'equal) 1) opts)) 0))
      (setq opts (remove "--maxnum" (remove (nth (+ (cl-position "--maxnum" opts :test 'equal) 1) opts) opts)))))
    (unless (or (member "-n" flags)  (member "--maxnum" flags))
      (if (and consult-mu-maxnum (> consult-mu-maxnum 0))
          (setq opts (append opts (list "--maxnum" (format "%s" consult-mu-maxnum))))))

    (pcase consult-mu-search-sort-direction
      ('descending
       (if (or (member "-z" flags) (member "--reverse" flags))
           (setq opts (remove "-z" (remove "--reverse" opts)))
         (setq opts (append opts (list "--reverse")))))
      ('ascending)
      (_))
    (pcase-let* ((`(,re . ,hl) (funcall consult--regexp-compiler arg 'basic t)))
      (when re
        (cons (append cmd
                      (list (string-join re " "))
                      opts)
              hl)))))

(defun consult-mu--async (prompt builder &optional initial)
  "Query mu4e messages asynchronously.

This is a non-interactive internal function.  For the interactive
version, see `consult-mu-async'.

It runs the command line from `consult-mu--async-builder' in an async
process and returns the results (list of messages) as a completion table
in minibuffer that will be passed to `consult--read'.  The completion
table gets dynamically updated as the user types in the minibuffer.  Each
candidate in the minibuffer is formatted by `consult-mu--async-transform'
to add annotation and other info to the candidate.

Description of Arguments:

PROMPT  the prompt in the minibuffer
        \(passed as PROMPT to `consult--red'\)
BUILDER an async builder function passed to `consult--async-command'
INITIAL an optional arg for the initial input in the minibuffer
        \(passed as INITITAL to `consult--read'\)

commandline arguments/options \(see `mu find --help` in the command line
for details\) can be passed to the minibuffer input similar to
`consult-grep'.  For example the user can enter:

“#paper -- --maxnum 200 --sortfield from --reverse”

this will search for mu4e messages with the query “paper”, retrives a
maximum of 200 messages sorts them by the “from:” field and reverses the
sort direction (opposite of `consult-mu-search-sort-field').

Also, the results can further be narrowed by
`consult-async-split-style' \(e.g. by entering “#” when
`consult-async-split-style' is set to \='perl\).

For example:

`#paper -- --maxnum 200 --sortfield from --reverse#accepted'

will retrieve the message as the example above, then narrows down the
completion table to candidates that match “accepted”."
  (consult--read
   (consult--process-collection builder
     :transform (consult--async-transform-by-input #'consult-mu--async-transform))
   :prompt prompt
   :lookup (consult-mu--lookup)
   :state (funcall #'consult-mu--async-state)
   :initial initial
   :group #'consult-mu--group
   :add-history (append (list (thing-at-point 'symbol))
                        consult-mu-saved-searches-async)
   :history '(:input consult-mu--history)
   :require-match t
   :category 'consult-mu-messages
   :preview-key consult-mu-preview-key
   :sort nil))

(defun consult-mu-async (&optional initial noaction)
  "Lists results of `mu find` Asynchronously.

This is an interactive wrapper function around `consult-mu--async'.  It
queries the user for a search term in the minibuffer, then fetches a list
of messages for the entered search term as a minibuffer completion table
for selection.  The list of candidates in the completion table are
dynamically updated as the user changes the entry.

Upon selection of a candidate either
 - the candidate is returned if NOACTION is non-nil
 or
 - the candidate is passed to `consult-mu-action' if NOACTION is nil.

Additional commandline arguments can be passed in the minibuffer entry by
typing `--` followed by command line arguments.

For example the user can enter:

`#consult-mu -- -n 10'

this will run a `mu4e-search' with the query \"consult-my\" and changes the
search limit (i.e. `mu4e-search-results-limit' to 10.


Also, the results can further be narrowed by `consult-async-split-style'
\(e.g. by entering “#” when `consult-async-split-style' is set to \='perl\).

For example:

“#consult-mu -- -n 10#github”

will retrieve the message as the example above, then narrows down the
completion table to candidates that match “github”.

INITIAL is an optional arg for the initial input in the minibuffer.
\(passed as INITITAL to `consult-mu--async'\).

For more details on consult--async functionalities, see `consult-grep' and
the official manual of consult, here:
URL `https://github.com/minad/consult'

Note that this is the async search directly using the commandline `mu`
command and not mu4e-search. As a result, mu4e-headers buffers are not
created until a single message is selected \(or interacted with using
embark, etc.\)  Previews are shown in a mu4e-view buffer \(see
`consult-mu-view-buffer-name'\) attached to an empty mu4e-headers buffer
\(i.e. `consult-mu-headers-buffer-name'\).  This allows quick retrieval of
many messages \(tens of thousands\) and previews, but not opening the
results in a mu4e-headers buffer.  If you want ot open the results in a
mu4e-headers buffer for other work flow, then you should use the
dynamically collected function `consult-mu' which is slower if searching
for many emails but allows follow up interactions in a mu4e-headers
buffer."
  (interactive)
  (save-mark-and-excursion
    (consult-mu--execute-all-marks))
  (let* ((sel
          (consult-mu--async (concat "[" (propertize "consult-mu async" 'face 'consult-mu-sender-face) "]" " Search For:  ") #'consult-mu--async-builder initial))
         (info (cdr sel))
         (msg (plist-get info :msg))
         (query (plist-get info :query)))
    (save-mark-and-excursion
      (consult-mu--execute-all-marks))
    (if noaction
        sel
      (progn
        (consult-mu--update-headers query t msg :async))
      (funcall consult-mu-action sel)
      sel)))

(defun consult-mu (&optional initial noaction)
  "Default interactive command.

This is a wrapper function that calls `consult-mu-default-command' with
INITIAL and NOACTION.

For example, the `consult-mu-default-command can be set to
 `#'consult-mu-dynamic' sets the default behavior to dynamic collection
 `#'consult-mu-async' sets the default behavior to async collection"

  (interactive "P")
  (funcall consult-mu-default-command initial noaction))

;;; provide `consult-mu' module
(provide 'consult-mu)

;;; consult-mu.el ends here
