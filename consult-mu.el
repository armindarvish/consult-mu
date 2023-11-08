;;; consult-mu.el --- Consult Mu4e asynchronously in GNU Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2023 Armin Darvish

;; Author: Armin Darvish
;; Maintainer: Armin Darvish
;; Created: 2023
;; Version: 0.1
;; Package-Requires: ((emacs "28.0") (consult "0.34") (mu "1.10"))
;; Homepage: https://github.com/armindarvish/consult-mu
;; Keywords: convenience, matching, tools, email

;;; Commentary:

;;; Code:

;;; Requirements
(eval-when-compile
(require 'consult)
(require 'mu4e)
)

;;; Group

(defgroup consult-mu nil
  "Consulting mu command"
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
  "Maximum number of results normally passed to \"--maxnum\" in the command line. "
  :group 'consult-mu
  :type '(choice (const :tag "Unlimited" -1)
                 (integer :tag "Limit"))
  )

(defcustom consult-mu-search-sort-field mu4e-search-sort-field
  "What field to sort results by "
  :group 'consult-mu
  :type '(radio (const :date)
                (const :subject)
                (const :size)
                (const :prio)
                (const :from)
                (const :to)
                (const :list)))

(defcustom consult-mu-headers-fields mu4e-headers-fields
  "A list of header fields to show in the headers buffer.
Each element has the form (HEADER . WIDTH), where HEADER is one of
the available headers (see `mu4e-header-info') and WIDTH is the
respective width in characters.

A width of nil means \"unrestricted\", and this is best reserved
for the rightmost (last) field. Note that emacs may become very
slow with excessively long lines (1000s of characters), so if you
regularly get such messages, you want to avoid fields with nil
altogether."
  :group 'consult-mu
  :type `(repeat (cons (choice ,@(mapcar (lambda (h)
                                           (list 'const :tag
                                                 (plist-get (cdr h) :help)
                                                 (car h)))
                                         mu4e-header-info))
                       (choice (integer :tag "width")
                               (const :tag "unrestricted width" nil)))))

(defcustom consult-mu-headers-string nil
  "A string to format headers"
  :group 'consult-mu
  :type 'string)

(defcustom consult-mu-search-sort-direction mu4e-search-sort-direction
  "Direction to sort by; a symbol
 `descending' (sorting
  Z->A)
or
`ascending' (sorting A->Z)."
  :group 'consult-mu
  :type '(radio (const ascending)
                (const descending)))

(defcustom consult-mu-group-by :date
  "What field to sort results by "
  :group 'consult-mu
  :type '(radio (const :date)
                (const :subject)
                (const :from)
                (const :to)
                (const :time)
                (const :datetime)
                ))

(defcustom consult-mu-mark-previewed-as-read nil
  "What field to sort results by "
  :group 'consult-mu
  :type 'boolean)

(defcustom consult-mu-mark-viewed-as-read t
  "What field to sort results by "
  :group 'consult-mu
  :type 'boolean)

(defcustom consult-mu-headers-buffer-name "*consult-mu-headers*"
  "Default name to use for preview buffers showing repo readmes retrieved by \"gh repo view\"."
  :group 'consult-mu
  :type 'string)

(defcustom consult-mu-view-buffer-name "*consult-mu-view*"
  "Default name to use for preview buffers showing repo readmes retrieved by \"gh repo view\"."
  :group 'consult-mu
  :type 'string)

(defcustom consult-mu-preview-key consult-preview-key
  "Preview key for consult-mu. This is similar `consult-preview-key' but explicitly for consult-mu."
  :type '(choice (const :tag "Any key" any)
                 (list :tag "Debounced"
                       (const :debounce)
                       (float :tag "Seconds" 0.1)
                       (const any))
                 (const :tag "No preview" nil)
                 (key :tag "Key")
                 (repeat :tag "List of keys" key)))


(defcustom consult-mu-highlight-matches t
  "This variable defines whether `consult-mu' highlights search queries (or code snippets) in preview buffers to visually guide the user see the most relevant content in afile."
  :group 'consult-mu
  :type 'boolean)

(defcustom consult-mu-action #'consult-mu--view-action
  "The function that is used when selecting a message. By default it is bound to `consult-mu--view-action'."
  :group 'consult-mu
  :type 'function)

;;; Other Variables
(defvar consult-mu-category 'consult-mu
  "Category symbol for the `consult-mu' package.")

(defvar consult-mu-messages-category 'consult-mu-messages
  "Category symbol for messages in `consult-mu' package.")

(defvar consult-mu--view-buffers-list (list)
  "List of currently open preview buffers")

(defvar consult-mu--history nil
  "History variable for `consult-mu'.")

(defvar consult-mu-delimiter "      "
  "Delimiter for fields in mu output.
Taken from  https://github.com/seanfarley/counsel-mu.")

(defvar consult-mu-saved-searches (list)
  "List of Favorite searches.")

;;; Faces

(defface consult-mu-highlight-match-face
  `((t :inherit 'consult-highlight-match))
  "highlight match face in `consult-mu''s preview buffers.
By default inherits from `consult-highlight-match'. "
  )

(defface consult-mu-preview-match-face
  `((t :inherit 'consult-preview-match))
  "highlight match face in `consult-mu''s preview buffers.
 By default inherits from `consult-preview-match'. This face is for example used to highlight the matches to the user's search queries in preview buffer.")

(defface consult-mu-default-face
  `((t :inherit 'default))
  "default face in `consult-mu''s minibuffer annotations.
By default inherits from `default'.")

(defface consult-mu-subject-face
  `((t :inherit 'font-lock-keyword-face))
  "Subject face in `consult-mu''s minibuffer annotations.
By default inherits from `font-lock-type-face'.")

(defface consult-mu-sender-face
  `((t :inherit 'font-lock-variable-name-face))
  "Contact face in `consult-mu''s minibuffer annotations.
By default inherits from `font-lock-constant-face'.")

(defface consult-mu-receiver-face
  `((t :inherit 'font-lock-variable-name-face))
  "Contact face in `consult-mu''s minibuffer annotations.
By default inherits from `font-lock-constant-face'.")

(defface consult-mu-date-face
  `((t :inherit 'font-lock-preprocessor-face))
  "date face in `consult-mu''s minibuffer annotations.
By default inherits from `font-lock-keyword-face'.")

(defface consult-mu-count-face
  `((t :inherit 'font-lock-string-face))
  "Count face in `consult-mu''s minibuffer annotations.
By default inherits from `font-lock-type-face'.")

(defface consult-mu-size-face
  `((t :inherit 'font-lock-string-face))
  "Count face in `consult-mu''s minibuffer annotations.
By default inherits from `font-lock-type-face'.")

(defface consult-mu-tags-face
  `((t :inherit 'font-lock-comment-face))
  "tags/comments face in `consult-mu''s minibuffer annotations.
By default inherits from `font-lock-comment-face'.")

(defface consult-mu-flags-face
  `((t :inherit 'font-lock-function-call-face))
  "tags/comments face in `consult-mu''s minibuffer annotations.
By default inherits from `font-lock-comment-face'.")

(defface consult-mu-url-face
  `((t :inherit 'link))
  "url face in `consult-mu''s minibuffer annotations; by default inherits from `link'.")

;;; Utility functions
(defun consult-mu--nonutf-cleanup (string)
"Remove non UTF-8 characters if any in the string."
  (string-join
   (delq nil (mapcar (lambda (ch) (encode-coding-char ch 'utf-8 'unicode))
                     string))))

(defun consult-mu--set-string-width (string width &optional prepend)
  "Sets the STRING width to a fixed value, WIDTH.
If the String is longer than WIDTH, it truncates the string and add an ellipsis, \"...\". If the string is shorter it adds whitespace to the string.
If PREPEND is non-nil, it truncates or adds whitespace from the beginning of string, instead of the end."
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
  "Sets the width of  STRING+PREFIX justified from left.
It uses `consult-mu--set-string-width' and sets the width of the concatenate of STRING+PREFIX (e.g. `(concat prefix string)`) within MAXWIDTH. This is used for aligning marginalia info in minibuffer when using `consult-mu'."
  (let ((s (string-width string))
        (w (string-width prefix)))
    (if (> maxwidth w)
    (consult-mu--set-string-width string (- maxwidth w) t)
    string
          )
    ))

(defun consult-mu--highlight-match (regexp str ignore-case)
  "Highlights REGEXP in STR.
If a regular expression contains capturing groups, only these are highlighted.
If no capturing groups are used highlight the whole match.  Case is ignored
if IGNORE-CASE is non-nil.
(This is adapted from `consult--highlight-regexps'.)"
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
                                     'consult-mu-highlight-match-face nil str)
            )
          (setq m (cddr m))))))
  str)

(defun consult-mu--overlay-match (match-str buffer ignore-case)
(with-current-buffer (or (get-buffer buffer) (current-buffer))
  (remove-overlays (point-min) (point-max) 'consult-mu-overlay t)
  (goto-char (point-min))
  (let ((case-fold-search ignore-case)
        (consult-mu-overlays (list)))
    (while (search-forward match-str nil t)
      (when-let* ((m (match-data))
                  (beg (car m))
                  (end (cadr m))
                  (overlay (make-overlay beg end))
                  )
        (overlay-put overlay 'consult-mu-overlay t)
        (overlay-put overlay 'face 'consult-mu-highlight-match-face)
        )))))

(defun consult-mu-overlays-toggle (&optional buffer)
(interactive)
(let ((buffer (or buffer (current-buffer))))
(with-current-buffer buffer
  (dolist (o (overlays-in (point-min) (point-max)))
    (when (overlay-get o 'consult-mu-overlay)
      (if (and (overlay-get o 'face) (eq (overlay-get o 'face) 'consult-mu-highlight-match-face))
          (overlay-put o 'face nil)
         (overlay-put o 'face 'consult-mu-highlight-match-face))
      )
))))

(defun consult-mu-format-candidate-string (string)
  (pcase string
    "%d" 'date
  ))

(defun consult-mu--format-date (string)
  (let ((string (replace-regexp-in-string " " "0" string)))
    (format "%s %s %s"
            (substring string 0 10)
            (substring string -4 nil)
            (substring string 11 -4)
            )))

(defun consult-mu--expand-headers-string (msg string)
  (cl-loop with str = nil
           for c in (string-split string "%" t)
           concat (concat (pcase  (substring c 0 1)
                            ("f" (let ((sender (mapconcat (lambda (item) (or (plist-get item :name) (plist-get item :email))) (plist-get msg :from) ";"))
                                       (length (string-to-number (substring c 1 nil))))
                                   (if sender
                                     (propertize (if (> length 0) (consult-mu--set-string-width sender length) sender) 'face 'consult-mu-sender-face))))
                            ("t" (let ((receiver (mapconcat (lambda (item) (or (plist-get item :name) (plist-get item :email))) (plist-get msg :to) ";"))
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

                            ("c" (let ((cc (mapconcat (lambda (item) (or (plist-get item :name) (plist-get item :email))) (plist-get msg :cc) ";"))
                                       (length (string-to-number (substring c 1 nil))))
                                   (if cc
                                     (propertize (if (> length 0) (consult-mu--set-string-width cc length) cc) 'face 'consult-mu-tags-face))))

                            ("h" (let ((bcc (mapconcat (lambda (item) (or (plist-get item :name) (plist-get item :email))) (plist-get msg :bcc) ";"))
                                       (length (string-to-number (substring c 1 nil))))
                             (if bcc
                                     (propertize (if (> length 0) (consult-mu--set-string-width bcc length) bcc) 'face 'consult-mu-tags-face))))

                            ("r" (let ((changed (format-time-string "%a %d %b %y" (plist-get msg :changed)))
                                       (length (string-to-number (substring c 1 nil))))
                                   (if changed
                                     (propertize (if (> length 0) (consult-mu--set-string-width changed length) changed) 'face 'consult-mu-tags-face))))
                            (_ nil)
                            ) "  ")))

(defun consult-mu-flags-to-string (FLAG)
  (cl-loop for c across FLAG
           collect
           (pcase (string c)
             ("D" "draft")
             ("F" "flagged")
             ("N" "new")
             ("P" "forwarded")
             ("R"  "replied")
             ("S" "read")
             ("T" "trashed")
             ("a" "attachment")
             ("x" "encrrypted")
             ("s" "signed")
             ("u" "unread")
             ("l" "list")
             ("q" "personal")
             ("c" "calendar")
             (_ ""))))

;;; Backend `mu` related functions

(defun consult-mu--call-process (&rest args)
 "Runs \"mu\" in the command line and passes ARGS as command-line arguments.
Returns a list where the CAR is exit status (e.g. 0 means success and non-zero means error) and CADR is the output's text. If mu is not found it returns '(127 \"\") and a message saying \"mu\" is not found."
(if (executable-find "mu")
      (with-temp-buffer
        (set-buffer-file-coding-system 'cp1047)
        (list (apply 'call-process "mu" nil (current-buffer) nil args)                         (replace-regexp-in-string "" "\n"                                                   (buffer-string))))
  (progn
      (message (propertize "\"mu\" is not found on this system" 'face 'warning))
      '(127 ""))
))

(defun consult-mu--command-to-string (&rest args)
  "Runs `consult-mu--call-process' and returns a string if there is no error.
If there are erros passes them to *Messages*."
  (let ((out (apply #'consult-mu--call-process args)))
          (if (= (car out) 0)
              (cadr out)
            (progn
              (message (cadr out))
              nil)
            )))

(defun consult-mu--headers-append-handler (msglst)
  "Append one-line descriptions of messages in MSGLIST.
Do this at the end of the headers-buffer.

Overrides `mu4e~headers-append-handler' for `consult-mu'."
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

(defun consult-mu--view-msg (msg)
  "Display the message MSG in a new buffer, and keep in sync with `consult-mu-headers-buffer-name' buffer.
\"In sync\" here means that moving to the next/previous message
in the the message view affects `consult-mu-headers-buffer-name', as does marking etc.

As a side-effect, a message that is being viewed loses its
`unread' marking if it still had that.

Overrides `mu4e-view' for `consult-mu'."
  ;; update headers, if necessary.
  (mu4e~headers-update-handler msg nil nil)
  (let* ((linked-headers-buffer (get-buffer "*consult-mu-headers*"))
         (mu4e-view-buffer-name "*consult-mu-view*"))
    (setq gnus-article-buffer (mu4e-get-view-buffer nil t))

    (with-current-buffer gnus-article-buffer
      (let ((inhibit-read-only t))
        (remove-overlays (point-min) (point-max) 'mu4e-overlay t)
        (erase-buffer)
        (insert-file-contents-literally
         (mu4e-message-readable-path msg) nil nil nil t)
        (setq-local mu4e--view-message msg)
        (mu4e--view-render-buffer msg))
      (mu4e-loading-mode 0)))

  (unless (mu4e--view-detached-p gnus-article-buffer)
    (with-current-buffer mu4e-linked-headers-buffer
      (setq-local mu4e~headers-view-win
                  (mu4e-display-buffer gnus-article-buffer nil)
                  )
      (unless (window-live-p mu4e~headers-view-win)
        (mu4e-error "Cannot get a message view"))
      ))

  (with-current-buffer gnus-article-buffer
    (let ((inhibit-read-only t))
      (run-hooks 'mu4e-view-rendered-hook)
      ;;replace the following with appropriate overlay highlight
      ;;(highlight-regexp mu4e--search-last-query 'consult-mu-preview-match-face)
      )))

(defun consult-mu--headers-clear (&optional text)
  "Clear the headers buffer and related data structures.
Optionally, show TEXT. Overrides `mu4e~headers-clear' for `consult-mu'."
    (setq mu4e~headers-render-start (float-time)
          mu4e~headers-hidden 0)
    (let ((inhibit-read-only t))
      (with-current-buffer "*consult-mu-headers*"
        (mu4e--mark-clear)
        (erase-buffer)
        (when text
          (goto-char (point-min))
          (insert (propertize text 'face 'mu4e-system-face 'intangible t))))))

(defun consult-mu--set-mu4e-search-sortfield (opts)
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
      (_
       consult-mu-search-sort-field)
      )))

(defun consult-mu--set-mu4e-search-sort-direction (opts)
  (if (or (member "-z" opts) (member "--reverse" opts))
      (pcase consult-mu-search-sort-direction
        ('descending
         'ascending)
        ('ascending
         'descending))
    consult-mu-search-sort-direction))

(defun consult-mu--set-mu4e-skip-duplicates (opts)
  (if (member "--skip-dups" opts) t mu4e-search-skip-duplicates))

(defun consult-mu--set-mu4e-results-limit (opts)
    (cond
     ((member "-n" opts) (string-to-number (nth (+ (cl-position "-n" opts :test 'equal) 1) opts)))
     ((member "--maxnum" opts) (string-to-number (nth (+ (cl-position "--maxnum" opts :test 'equal) 1) opts)))
     (t consult-mu-maxnum))
  )

(defun consult-mu--set-mu4e-threads (opts)
(if (not (equal mu4e-search-sort-field :date)) 'nil 't))

(defun consult-mu--update-headers (query ignore-history msgid)
  "Search for QUERY.
Update `consult-mu-headers-buffer-name' but do not switch to buffer.

If IGNORE-HISTORY is true, do *not* update the query history stack, `mu4e--search-query-past'.

Put cursor on message with MSGID."
(cl-letf* (((symbol-function #'mu4e~headers-append-handler) #'consult-mu--headers-append-handler))
    (unless (mu4e-running-p) (mu4e--server-start))
    (let* ((buf (mu4e-get-headers-buffer consult-mu-headers-buffer-name t))
           (inhibit-read-only t)
           (expr (car (consult--command-split query)))
           (rewritten-expr (funcall mu4e-query-rewrite-function expr))
           (maxnum (unless mu4e-search-full mu4e-search-results-limit))
           (mu4e-headers-fields consult-mu-headers-fields)
           )
      (with-current-buffer buf
        (save-excursion
          (erase-buffer)
          (mu4e-headers-mode)
          (setq-local mu4e-view-buffer-name consult-mu-view-buffer-name)
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
          (pcase-let* ((`(,arg . ,opts) (consult--command-split query))
                       (mu4e-search-sort-field (consult-mu--set-mu4e-search-sortfield opts))
                       (mu4e-search-sort-direction (consult-mu--set-mu4e-search-sort-direction opts))
                       (mu4e-search-skip-duplicates (consult-mu--set-mu4e-skip-duplicates opts))
                       (mu4e-search-results-limit (consult-mu--set-mu4e-results-limit opts))
                       (mu4e-search-threads (consult-mu--set-mu4e-threads opts))
                       )
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
                     (not (or (equal (buffer-substring (- (point-max) (length mu4e~no-matches)) (point-max)) mu4e~no-matches) (equal (buffer-substring (- (point-max) (length mu4e~end-of-results)) (point-max)) mu4e~end-of-results)))
                     )
            (sleep-for 0.005)
            )
          )
        )))
  (unless inhibit-read-only (setq inhibit-read-only t))
  )

(defun consult-mu--update-view (msgid mark-as-read match-str)
  "Open the message with MSGID in `consult-mu-view-buffer-name'."
  (cl-letf* (((symbol-function #'mu4e-view) #'consult-mu--view-msg))
    (when-let ((buffer (get-buffer consult-mu-view-buffer-name)))
    (with-current-buffer buffer
       (let ((inhibit-read-only t))
        (erase-buffer)
        )))
      (with-current-buffer consult-mu-headers-buffer-name
        (ignore-errors (mu4e-headers-goto-message-id msgid))
              (mu4e--server-call-mu
               `(view
                 :docid nil
                 :msgid ,msgid
                 :mark-as-read ,mark-as-read
                 :rename  ,(and mu4e-change-filenames-when-moving t)
                 ))
              ;; wait until the view buffer is updated. Otherwise the cl-letf override is reversed and nothing is shown!
              (while (or (not (get-buffer consult-mu-view-buffer-name))
                         (with-current-buffer consult-mu-view-buffer-name
                           (or
                           (string-empty-p (buffer-substring (point-min) (point-max)))
                           (equal (buffer-substring (point-min) (+ (point-min) (length "Loading..."))) "Loading...")
                           )))
                (sleep-for 0.005))
              ;; if the headers buffer is open, bury it, so does not take space when previewing messages!
              ;;(bury-buffer)
              )
              ;;(goto-char (point-min))
      (when match-str
        (add-to-history 'search-ring match-str)
      (consult-mu--overlay-match match-str consult-mu-view-buffer-name t))
      )
  ;; make sure minibuffer is not in read-only!
  (unless inhibit-read-only (setq inhibit-read-only t))
  )

(defun consult-mu--execute-all-marks (&optional no-confirmation)
  (interactive "P")
  (when-let* ((buf (get-buffer consult-mu-headers-buffer-name)))
    (with-current-buffer buf
      (pop-to-buffer buf)
      (unless (one-window-p) (delete-other-windows))
      (mu4e--mark-in-context
       (let* ((marknum (mu4e-mark-marks-num)))
         (unless (zerop marknum)
           (mu4e-mark-execute-all no-confirmation))))
      (quit-window)
      ))
  )

;; (defun consult-mu--view (msgid match-str mark-as-read)
;;   (consult-mu--update-view msgid mark-as-read)
;;   (with-current-buffer consult-mu-view-buffer-name
;;     (unless (one-window-p) (delete-other-windows))
;;         )
;;     (unless inhibit-read-only (setq inhibit-read-only t))
;;     consult-mu-view-buffer-name
;;     )

(defun consult-mu-headers-goto-message-id (msgid)
  (when-let ((buffer consult-mu-headers-buffer-name))
    (with-current-buffer buffer
      (setq mu4e-view-buffer-name consult-mu-view-buffer-name)
      (mu4e-headers-goto-message-id msgid))))

(defun consult-mu--get-message-by-id (msgid)
  (cl-letf* (((symbol-function #'mu4e-view) #'consult-mu--view-msg))
  (when-let ((buffer consult-mu-headers-buffer-name))
    (with-current-buffer buffer
      (setq mu4e-view-buffer-name consult-mu-view-buffer-name)
      (mu4e-headers-goto-message-id msgid)
      (mu4e-message-at-point)
      ;;(mu4e-get-view-buffer)
      ;;(mu4e-headers-view-message)
     ;; (mu4e-view (mu4e-message-at-point))
      )
  ;;(unless inhibit-read-only (setq inhibit-read-only t))
  ;; (switch-to-buffer buffer)
  ;; (delete-other-windows)
  )))

(defun consult-mu--quit-header-buffer ()
  (save-mark-and-excursion
  (when-let* ((buf (get-buffer consult-mu-headers-buffer-name)))
    (with-current-buffer buf
      (if (eq major-mode 'mu4e-headers-mode)
          (mu4e-mark-handle-when-leaving)
        (quit-window t)
        ;; clear the decks before going to the main-view
        (mu4e--query-items-refresh 'reset-baseline)
        )))))

(defun consult-mu--quit-view-buffer ()
  (when-let* ((buf (get-buffer consult-mu-view-buffer-name)))
    (with-current-buffer buf
      (if (eq major-mode 'mu4e-view-mode)
          (mu4e-view-quit)
        ))))

(defun consult-mu--quit-main-buffer ()
  (when-let* ((buf (get-buffer mu4e-main-buffer-name)))
    (with-current-buffer buf
      (if (eq major-mode 'mu4e-main-mode)
          (mu4e-quit)
        ))))

(defun consult-mu--format-candidate (cand highlight)
  "Formats minibuffer candidates.

INPUT is the query from the user.

if HIGHLIGHT is t, input is highlighted with `consult-mu-highlight-match-face' in the minibuffer."

  (let* ((string (car cand))
         (info (cadr cand))
         (msg (plist-get info :msg))
         (query (plist-get info :query))
         (match-str (if (stringp query) (consult--split-escaped (car (consult--command-split query))) nil))
         (str (if consult-mu-headers-string
                 (consult-mu--expand-headers-string msg consult-mu-headers-string)
                  string)
         ))
         (if (and consult-mu-highlight-matches highlight)
                     (cond
                      ((listp match-str)
                       (mapcar (lambda (match) (setq str (consult-mu--highlight-match match str t))) match-str))
                      ((stringp match-str)
                       (setq str (consult-mu--highlight-match match-str str t))))
                   str)

    ;; (cons (propertize str :msg msg :query query :datetime datetime :date date :year year :month month :day day :week week :time time :msgid msgid :subject subject :from sender :to receiver :size size :tags tags) `(:msg ,msg :query ,query :datetime ,datetime :date ,date :year ,year :month ,month :day ,day :week ,week :time ,time :msgid ,msgid :subject ,subject :from ,sender :to ,receiver :size ,size :tags tags))
    (cons (propertize str :msg msg :query query) `(:msg ,msg :query ,query))
    ;; (print (format "%s" query))
    ;;(list str info)
    ))

(defun consult-mu--lookup ()
"Lookup function for repo candidates in consult-mu.
This is passed as LOOKUP to `consult--read' on candidates and is used to format the output when a candidate is selected."
  (lambda (sel cands &rest args)
    (let* ((info (cdr (assoc sel cands)))
           (msg  (plist-get info :msg))
           (subject (plist-get msg :subject))
           ;;(title (plist-get (plist-get (car info) :msg) :subject))
           ;;(title (plist-get (car info) :subject))
           )
      (cons subject info
      ))))

(defun consult-mu--state ()
  "State function for consult-mu candidates.
This is passed as STATE to `consult--read' and is used to preview or do other actions on the candidate."
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
                        (consult-mu--view msgid nil consult-mu-mark-previewed-as-read match-str)
                        )
               (with-current-buffer consult-mu-view-buffer-name
                 (unless (one-window-p) (delete-other-windows))
                 ))))
        ('return
         cand)
        ))))

(defun consult-mu--group-name (cand)
(if consult-mu-group-by
(let ((msg (get-text-property 0 :msg cand))
      (field (if (not (keywordp consult-mu-group-by)) (intern (concat ":" (format "%s" consult-mu-group-by))) consult-mu-group-by)))
      (pcase field
        (:date (format-time-string "%a %d %b %y" (plist-get msg field)))
        (:from (mapconcat (lambda (item) (or (plist-get item :name) (plist-get item :email))) (plist-get msg field) ";"))
        (:to (mapconcat (lambda (item) (or (plist-get item :name) (plist-get item :email))) (plist-get msg field) ";"))
        (:changed (format-time-string "%a %d %b %y" (plist-get msg field)))
        (:datetime (format-time-string "%F %r" (plist-get msg :date)))
        (:time (format-time-string "%X" (plist-get msg :date)))
        (:year (format-time-string "%Y" (plist-get msg :date)))
        (:month (format-time-string "%B" (plist-get msg :date)))
        (:day-of-week (format-time-string "%A" (plist-get msg :date)))
        (:week (format-time-string "%V" (plist-get msg :date)))
        (:size (file-size-human-readable (plist-get msg field)))
        (:flags (format "%s" (plist-get msg field)))
        (:tags (format "%s" (plist-get msg field)))
        (_ (format "%s" (plist-get msg field)))))))

(defun consult-mu--group (cand transform)
  "Group candidates in minibuffer for consult-mu.
This is passed as GROUP to `consult--read' and is used to group emails by date."
  (when-let ((name (consult-mu--group-name cand)))
    (if transform (substring cand) name)
    ))

(defun consult-mu--view (msgid select mark-as-read match-str)
  "Opens message with MSGID in `consult-mu-headers' and `consult-mu-view'."
  (cl-letf* (((symbol-function #'mu4e-view) #'consult-mu--view-msg))
    (when-let ((buf consult-mu-headers-buffer-name))
      (with-current-buffer buf
        (setq mu4e-view-buffer-name consult-mu-view-buffer-name)
        (mu4e-headers-goto-message-id msgid)
        (consult-mu--update-view msgid mark-as-read match-str)
        (if select
        (switch-to-buffer buf))
        )
      (with-current-buffer consult-mu-view-buffer-name
        (goto-char (point-min)))
      (if select
          (select-window (get-buffer-window consult-mu-view-buffer-name)))
      ))
  consult-mu-view-buffer-name)

(defun consult-mu--view-action (cand)
  "Opens the canidate, CAND, from consult-mu.

This is a wrapper function around `consult-mu--view'. It parses CAND to extract relevant msgid and passes them to `consult-mu--view'.

To use this as the default action for consult-mu, set `consult-mu-default-action' to #'consult-mu--view-action."

  (let* ((info (cdr cand))
         (msg (plist-get info :msg))
         (msgid (substring-no-properties (plist-get msg :message-id)))
         (query (substring-no-properties (plist-get info :query)))
         (match-str (car (consult--command-split query)))
         )
    (consult-mu--view msgid t consult-mu-mark-viewed-as-read match-str)
    (consult-mu-overlays-toggle consult-mu-view-buffer-name)
    ))

;; (defun consult-mu--transform (async builder)
;;   "Adds annotation to minibuffer candiates for `consult-mu'.

;; Returns ASYNC function after formating results with `consult-mu--format-candidate'.
;; BUILDER is the command line builder function (e.g. `consult-mu--builder')."
;;   (let ((input)
;;         (buffer (mu4e-get-headers-buffer consult-mu-headers-buffer-name t)))
;;     `(lambda (action)
;;        (cond
;;         ((stringp action)
;;          (setq input action)
;;          (with-current-buffer ,buffer
;;            ;;set mu4e variables so the headers buffer matches the results from commandline `mu` search
;;            (pcase-let* ((`(,arg . ,opts) (consult--command-split action))
;;                         (sortfield (cond
;;                                     ((member "-s" opts) (nth (+ (cl-position "-s" opts :test 'equal) 1) opts))
;;                                     ((member "--sortfield" opts) (nth (+ (cl-position "--sortfield" opts :test 'equal) 1) opts))
;;                                     (t consult-mu-search-sort-field)))
;;                         (mu4e-search-sort-field
;;                          (pcase sortfield
;;                            ('nil
;;                             consult-mu-search-sort-field)
;;                            ((or "date" "d")
;;                             :date)
;;                            ((or "subject" "s")
;;                             :subject)
;;                            ((or "size" "z")
;;                             :size)
;;                            ((or "prio" "p")
;;                             :prio)
;;                            ((or "from" "f")
;;                             :from)
;;                            ((or "to" "t")
;;                             :to)
;;                            ((or "list" "v")
;;                             :list)
;;                            (_
;;                             consult-mu-search-sort-field)
;;                            ))
;;                          (mu4e-search-sort-direction
;;                           (if
;;                               (or (member "-z" opts) (member "--reverse" opts))
;;                               (pcase consult-mu-search-sort-direction
;;                                 ('descending
;;                                  'ascending)
;;                                 ('ascending
;;                                  'descending))
;;                             consult-mu-search-sort-direction))
;;                          (mu4e-search-skip-duplicates
;;                           (if (member "--skip-dups" opts) t mu4e-search-skip-duplicates))
;;                          (mu4e-search-results-limit
;;                           (cond
;;                            ((member "-n" opts) (string-to-number (nth (+ (cl-position "-n" opts :test 'equal) 1) opts)))
;;                            ((member "--maxnum" opts) (string-to-number (nth (+ (cl-position "--maxnum" opts :test 'equal) 1) opts)))
;;                            (t consult-mu-maxnum)))
;;                          (mu4e-search-threads
;;                           (if (not (equal mu4e-search-sort-field :date)) nil t))
;;                          )
;;                         (consult-mu--update-headers arg nil nil)))
;;            (funcall ,async action)
;;            )
;;          (t (mapcar (lambda (string)
;;                       (consult-mu--format-candidate string input t))
;;                     (funcall ,async action)))
;;          ))))


(defun consult-mu--transform (async builder)
  "Adds annotation to minibuffer candiates for `consult-mu'.

Returns ASYNC function after formating results with `consult-mu--format-candidate'.
BUILDER is the command line builder function (e.g. `consult-mu--builder')."
  (let ((input)
        (buffer (mu4e-get-headers-buffer consult-mu-headers-buffer-name t)))
    `(lambda (action)
       (cond
        ((stringp action)
         (setq input action)
         (with-current-buffer ,buffer
           (consult-mu--update-headers action nil nil))
           (funcall ,async action)
           )
         (t (mapcar (lambda (string)
                      (consult-mu--format-candidate string input t))
                    (funcall ,async action)))
         ))))

(defun consult-mu--builder (input)
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
    (setq opts (append opts (list "--nocolor")))
    (setq opts (append opts (list "--fields" (format "i%sd%sf%st%ss%sz%sg%sx"
                                                     consult-mu-delimiter consult-mu-delimiter consult-mu-delimiter consult-mu-delimiter consult-mu-delimiter consult-mu-delimiter consult-mu-delimiter))))
    (unless (or (member "-s" flags) (member "--sortfiled" flags))
    (setq opts (append opts (list "--sortfield" (substring (symbol-name consult-mu-search-sort-field) 1)))))
    (if threads (setq opts (append opts (list "--thread"))))
    (if skip-dups (setq opts (append opts (list "--skip-dups"))))
    (unless (or (member "-n" flags) (member "--maxnum" flags))
      (if (> consult-mu-maxnum 0)
          (setq opts (append opts (list "--maxnum" (format "%s" consult-mu-maxnum))))))
    (pcase consult-mu-search-sort-direction
      ('descending
       (if (or (member "-z" flags) (member "--reverse" flags))
           (setq opts (remove "-z" (remove "--reverse" opts)))
         (setq opts (append opts (list "--reverse")))))
      ('ascending
       )
      (_
       )
      )
    (pcase-let* ((`(,re . ,hl) (funcall consult--regexp-compiler arg 'basic t)))
      (when re
        (cons (append cmd
                      (list (string-join re " "))
                      opts)
              hl)))))

(defun consult-mu-get-candidates (input)
(let ((msg (ignore-errors (mu4e-message-at-point))))
  `(,(buffer-substring (point) (point-at-eol)) (:msg ,(ignore-errors (mu4e-message-at-point)) :query ,input))))

(defun consult-mu--dynamic-collection (input)
    ;; Somehow generate candidates, e.g., via org-ql
(save-excursion
  (consult-mu--update-headers input nil nil)
    (with-current-buffer consult-mu-headers-buffer-name
      (goto-char (point-min))
      (remove nil
      (cl-loop until (eobp)
               collect (when-let ((msg (ignore-errors (mu4e-message-at-point)))
                             (match-str (if (stringp input) (consult--split-escaped (car (consult--command-split input))) nil)))
                                                                    (consult-mu--format-candidate `(,(buffer-substring (point) (point-at-eol)) (:msg ,(ignore-errors (mu4e-message-at-point)) :query ,input)) t))
                 do (forward-line 1)))
        )))

(defun consult-mu--async (prompt collection &optional initial)
  "Query mu4e messages asynchronously.

This is a non-interactive internal function. For the interactive version see `consult-mu'.

It runs the command line from `consult-mu--builder' in an async process and returns the results (list of messages) as a completion tabe in minibuffer that will be passed to `consult--read'. The completion table gets dynamically updated as the user types in the minibuffer. Each candidate in the minibuffer is formatted by `consult-mu--transform' to add annotation and other info to the candidate.

PROMPT is the prompt in the minibuffer (passed as PROMPT to `consult--red'.)
BUILDER is an async builder function passed to `consult--async-command'.
INITIAL is an optional arg for the initial input in the minibuffer. (passed as INITITAL to `consult--read'.)
"
  (consult--read
   (consult--dynamic-collection collection)
   :prompt prompt
   :lookup (consult-mu--lookup)
   :state (funcall #'consult-mu--state)
   :initial (consult--async-split-initial initial)
   :group (if consult-mu-group-by #'consult-mu--group nil)
   :add-history (append (list (consult--async-split-thingatpt 'symbol))
                        consult-mu-saved-searches
                        )
   :history '(:input consult-mu--history)
   :require-match t
   :category 'consult-mu-messages
   :preview-key consult-mu-preview-key
   :sort nil))

(defun consult-mu (&optional initial noaction)
    "Lists results of `mu find` Asynchronously.

This is an interactive wrapper function around `consult-mu--async'. It queries the user for a search term in the minibuffer, then fetches a list of messages for the entered search term as a minibuffer completion table for selection. The list of candidates in the completion table are dynamically updated as the user changes the entry.

Upon selection of a candidate either
 - the candidate is returned if NOACTION is non-nil
 or
 - the candidate is passed to `consult-mu-action' if NOACTION is nil.

Additional commandline arguments can be passed in the minibuffer entry by typing `--` followed by command line arguments. For example the user can enter the following in the minibuffer:
consult-mu -- -n 10
and the async process will run `mu find -n 10` which changes the limit for the maximum number of results to 10.

INITIAL is an optional arg for the initial input in the minibuffer. (passed as INITITAL to `consult-mu--async').

For more details on consult--async functionalities, see `consult-grep' and the official manual of consult, here: https://github.com/minad/consult.
"
  (interactive)
  (save-mark-and-excursion
  (consult-mu--execute-all-marks)
  )
  (let* ((sel
        (consult-mu--async "Search For:  " #'consult-mu--dynamic-collection initial)
         ))
    (save-mark-and-excursion
      (consult-mu--execute-all-marks)
      )
    (if noaction
        sel
      (progn
        (funcall consult-mu-action sel)
        sel))))

;;; provide `consult-mu' module

(provide 'consult-mu)

;;; filename ends here
