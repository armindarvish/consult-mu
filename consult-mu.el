;;; consult-mu.el --- Consult Mu4e asynchronously in GNU Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2023 Armin Darvish

;; Author: Armin Darvish
;; Maintainer: Armin Darvish
;; Created: 2023
;; Version: 0.1
;; Package-Requires: ((emacs "28.0") (consult "0.34") (mu "1.10.7"))
;; Homepage: https://github.com/armindarvish/consult-mu
;; Keywords: convenience, matching, tools, email

;;; Commentary:

;;; Code:

;;; Requirements
(eval-when-compile
(require 'consult)
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

(defcustom consult-mu-maxnum 100
  "Maximum number of results normally passed to \"--maxnum\" in the command line. "
  :group 'consult-mu
  :type 'integer)

(defcustom consult-mu-sortfield "date"
  "What field to sort results by "
  :group 'consult-mu
  :type 'string)

(defcustom consult-mu-group-by 'date
  "What field to sort results by "
  :group 'consult-mu
  :type 'symbol)

(defcustom consult-mu-preview-buffer-name "*consult-mu-preview*"
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
  "This variable defines the function that is used when selecting a message. By default it is bound to `consult-mu--view-action'."
  :group 'consult-mu
  :type 'function)

;;; Other Variables
(defvar consult-mu-category 'consult-mu
  "Category symbol for the `consult-mu' package.")

(defvar consult-mu--preview-buffers-list (list)
  "List of currently open preview buffers")

(defvar consult-mu--history nil
  "History variable for `consult-mu'.")

(defvar consult-mu-delimiter "  "
  "Delimiter for fields in mu output.
Taken from  https://github.com/seanfarley/counsel-mu.")

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
  `((t :inherit 'font-lock-type-face))
  "Subject face in `consult-mu''s minibuffer annotations.
By default inherits from `font-lock-type-face'.")

(defface consult-mu-sender-face
  `((t :inherit 'font-lock-constant-face))
  "Contact face in `consult-mu''s minibuffer annotations.
By default inherits from `font-lock-constant-face'.")

(defface consult-mu-receiver-face
  `((t :inherit 'font-lock-constant-face))
  "Contact face in `consult-mu''s minibuffer annotations.
By default inherits from `font-lock-constant-face'.")

(defface consult-mu-date-face
  `((t :inherit 'font-lock-keyword-face))
  "date face in `consult-mu''s minibuffer annotations.
By default inherits from `font-lock-keyword-face'.")

(defface consult-mu-count-face
  `((t :inherit 'font-lock-type-face))
  "Count face in `consult-mu''s minibuffer annotations.
By default inherits from `font-lock-type-face'.")

(defface consult-mu-tags-face
  `((t :inherit 'font-lock-comment-face))
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

;; (defun consult-mu--justify-left (string prefix maxwidth)
;;   "Sets the width of  STRING+PREFIX justified from left.
;; It uses `consult-mu--set-string-width' and sets the width of the concatenate of STRING+PREFIX (e.g. `(concat prefix string)`) within MAXWIDTH. This is used for aligning marginalia info in minibuffer when using `consult-mu'."
;;   (let ((s (string-width string))
;;         (w (string-width prefix)))
;;     (cond ((< (+ s w) (floor (/ maxwidth 2)))
;;            (consult-mu--set-string-width string (- (floor (/ maxwidth 2))  w) t))
;;           ((< (+ s w) (floor (/ maxwidth 1.8)))
;;            (consult-mu--set-string-width string (- (floor (/ maxwidth 1.8))  w) t))
;;           ((< (+ s w) (floor (/ maxwidth 1.6)))
;;            (consult-mu--set-string-width string (- (floor (/ maxwidth 1.6))  w) t))
;;           ((< (+ s w) (floor (/ maxwidth 1.4)))
;;            (consult-mu--set-string-width string (- (floor (/ maxwidth 1.4)) w) t))
;;           ((< (+ s w) (floor (/ maxwidth 1.2)))
;;            (consult-mu--set-string-width string (- (floor (/ maxwidth 1.2)) w) t))
;;           ((< (+ s w) maxwidth)
;;            (consult-mu--set-string-width string (- maxwidth w) t))
;;           (t string)
;;           )
;;     ))

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
                                    'consult-mu-highlight-match-face nil str))
          (setq m (cddr m))))))
  str)

(defun consult-mu--format-date (string)
  (format "%s %s %s"
          (substring string 0 10)
          (substring string -4 nil)
          (substring string 11 -4)
))

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

(defun consult-mu--format-candidate (string input highlight)
  "Formats minibuffer candidates.

INPUT is the query from the user.

if HIGHLIGHT is t, input is highlighted with `consult-mu-highlight-match-face' in the minibuffer."

  (let* ((parts (string-split string consult-mu-delimiter))
         (msgid (car parts))
         (datetime (consult-mu--format-date (cadr parts)))
         (date (substring datetime 0 15))
         (time (substring datetime 16 nil))
         (sender (cadr (cdr parts)))
         (subject (cadr (cdr (cdr parts))))
         (query input)
         (match-str (if (stringp input) (consult--split-escaped (car (consult--command-split query))) nil))
         (str (format "%s\s\s%s\s\s%s"
                      (propertize (consult-mu--set-string-width subject (floor (* (frame-width) 0.6))) 'face 'consult-mu-subject-face)
                      (propertize sender 'face 'consult-mu-sender-face)
                      (propertize (consult-mu--format-date date) 'face 'consult-mu-date-face)))
         (str (propertize str :msgid msgid :subject subject :sender sender :datetime datetime :date date :time time :query query))
         )
    (if (and consult-mu-highlight-matches highlight)
        (cond
         ((listp match-str)
          (mapcar (lambda (match) (setq str (consult-mu--highlight-match match str t))) match-str))
         ((stringp match-str)
          (setq str (consult-mu--highlight-match match-str str t))))
      str)
    (cons str (list :msgid msgid :subject subject :sender sender :datetime datetime :date date :time time :query query))))

(defun consult-mu--lookup ()
"Lookup function for repo candidates in consult-mu.
This is passed as LOOKUP to `consult--read' on candidates and is used to format the output when a candidate is selected."
  (lambda (sel cands &rest args)
    (let* ((info (cdr (assoc sel cands)))
           (msgid (plist-get info :msgid)))
    (cons (format "%s" msgid) info))))

(defun consult-mu--state ()
"State function for consult-mu candidates.

This is passed as STATE to `consult--read' and is used to preview or do other actions on the candidate."
  (lambda (action cand)
    (let* ((preview (consult--buffer-preview))
           )
      (pcase action
            ('preview
             (if cand
                 (when-let ((repo (plist-get (cdr cand) :repo))
                            (query (plist-get (cdr cand) :query))
                            (match-str (consult--build-args query))
                       (buffer (get-buffer-create consult-mu-preview-buffer-name)))
                   (add-to-list 'consult-mu--preview-buffers-list buffer)
                   (consult-mu--repo-view (format "%s" repo) buffer)
                   (with-current-buffer buffer
                     (if consult-mu-highlight-matches
                     (cond
                      ((listp match-str)
                       (mapcar (lambda (item)
                                 (highlight-regexp item 'consult-mu-preview-match-face)) match-str))
                      ((stringp match-str)
                        (highlight-regexp match-str 'consult-mu-preview-match-face))
                      )))
               (funcall preview action
                       buffer
                        )
                   )

             ))
            ('return
             cand)
             )))
      )

(defun consult-mu--group-name (cand)
(get-text-property 0 (if (not (keywordp consult-mu-group-by))  (intern (concat ":" (format "%s" consult-mu-group-by))) consult-mu-group-by)  cand))

(defun consult-mu--group (cand transform)
  "Group candidates in minibuffer for consult-mu.
This is passed as GROUP to `consult--read' and is used to group emails by date."
  (let ((name (consult-mu--group-name cand)))
    (if transform (substring cand) (substring cand))
    ))

(defun consult-mu--view (msgid)
  "Opens message with MSGID in `mu4e-headers-view'."
(mu4e-view-message-with-message-id
   msgid))

(defun consult-mu--view-action (cand)
"Opens the canidate, CAND, from consult-mu.

This is a wrapper function around `consult-mu--view'. It parses CAND to extract relevant msgid and passes them to `consult-mu--view'.

To use this as the default action for consult-mu, set `consult-mu-default-action' to #'consult-mu--view-action."

    (let* ((info (cdr cand))
           (msgid (substring-no-properties (plist-get info :msgid))))
      (consult-mu--view msgid)
      ))

(defun consult-mu--transform (async builder)
  "Adds annotation to minibuffer candiates for `consult-mu'.

Returns ASYNC function after formating results with `consult-mu--format-candidate'.
BUILDER is the command line builder function (e.g. `consult-mu--builder')."
  (let (input)
    `(lambda (action)
      (cond
       ((stringp action)
        (setq input action)
        (funcall ,async action))
       (t (mapcar (lambda (string)
          (consult-mu--format-candidate string input t))
        (funcall ,async action)))
       ))))

(defun consult-mu--builder (input)
  "Build mu command line for searching messages by INPUT (e.g. `mu find INPUT)`."

  (pcase-let* ((consult-mu-args (append consult-mu-args '("find")))
               (cmd (consult--build-args consult-mu-args))
               (`(,arg . ,opts) (consult--command-split input))
               (flags (append cmd opts)))
    (setq opts (append opts (list "--skip-dups" "--nocolor")))
    (setq opts (append opts (list "--fields" (format "i%sd%sf%ss"
                                  consult-mu-delimiter consult-mu-delimiter consult-mu-delimiter))))
    (unless (or (member "-n" flags) (member "--maxnum" flags))
      (setq opts (append opts (list "--maxnum" (format "%s" consult-mu-maxnum)))))
    (unless (or (member "-s" flags) (member "--sortfiled" flags))
      (setq opts (append opts (list "--sortfield" (format "%s" consult-mu-sortfield)))))
    (unless (or (member "-z" flags) (member "--reverse" flags))
      (setq opts (append opts (list "--reverse"))))
    (pcase-let* ((`(,re . ,hl) (funcall consult--regexp-compiler arg 'basic t)))
      (when re
        (cons (append cmd
                      (list (string-join re " "))
                      opts)
              hl)))))

(defun consult-mu--async (prompt builder &optional initial)
"Query mu4e messages asynchronously.

This is a non-interactive internal function. For the interactive version see `consult-mu'.

It runs the command line from `consult-mu--builder' in an async process and returns the results (list of messages) as a completion tabe in minibuffer that will be passed to `consult--read'. The completion table gets dynamically updated as the user types in the minibuffer. Each candidate in the minibuffer is formatted by `consult-mu--transform' to add annotation and other info to the candidate.

PROMPT is the prompt in the minibuffer (passed as PROMPT to `consult--red'.)
BUILDER is an async builder function passed to `consult--async-command'.
INITIAL is an optional arg for the initial input in the minibuffer. (passed as INITITAL to `consult--read'.)
"

  (consult--read
   (consult--async-command builder
     (consult-mu--transform builder)
     )
   :prompt prompt
   :lookup (consult-mu--lookup)
   ;;:state (funcall #'consult-mu--state)
   :initial (consult--async-split-initial initial)
   :group #'consult-mu--group
   :add-history (list (consult--async-split-thingatpt 'symbol))
   :history '(:input consult-mu--history)
   :require-match t
   :category 'consult-mu
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
  (let ((sel
         (consult-mu--async "Search For:  " #'consult-mu--builder initial)))
    (if noaction
        sel
      (progn
        (funcall consult-mu-action sel)
        sel))))

;;; provide `consult-mu' module

(provide 'consult-mu)

;;; filename ends here
