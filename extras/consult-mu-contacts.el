;;; consult-mu-contacts.el --- Consult Mu4e asynchronously in GNU Emacs -*- lexical-binding: t -*-

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

(defcustom consult-mu-contacts-group-by :name
  "What field to use to group the results in the minibuffer.

By default it is set to :name. But can be any of:

  :name         group by contact name
  :email        group by email of the contact
  :domain       group by the domain of the contact's email (e.g. domain.com in user@domain.com)
  :user         group by the ncontact's user name (e.g. user in user@domain.com)
"
  :group 'consult-mu
  :type '(radio (const :name)
                (const :email)
                (const :domain)
                (const :user)))

(defcustom consult-mu-contacts-action #'consult-mu-contacts--list-messages-action
  "The function that is used when selecting a contact.
By default it is bound to `consult-mu-contacts--list-messages-action'."
  :group 'consult-mu
  :type 'function)

;;; Other Variables

(defvar consult-mu-contacts--override-group nil
"Override grouping in `consult-mu-contacs' based on user input.")

(defun consult-mu-contacts--list-messages (contact)
  (let* ((consult-mu-maxnum nil)
        (email (plist-get contact :email))
        )
      (consult-mu-dynamic (format "contact:%s" email))
))

(defun consult-mu-contacts--list-messages-action (cand)
  "Searches the messages from contact candidate, CAND.

This is a wrapper function around `consult-mu-contacts--list-messages'. It parses CAND to extract relevant CONTACT plist and other information and passes them to `consult-mu-contacts--list-messages'.

To use this as the default action for consult-mu-contacts, set `consult-mu-contacts-default-action' to #'consult-mu-contacts--list-messages-action."

  (let* ((info (cdr cand))
         (contact (plist-get info :contact))
         )
    (consult-mu-contacts--list-messages contact)
    ))

(defun consult-mu-contacts--format-candidate (string input highlight)
  "Formats minibuffer candidates for `consult-mu-contacts'.
STRING is the output retrieved from `mu find INPUT ...` in the command line.
INPUT is the query from the user.
if HIGHLIGHT is t, input is highlighted with `consult-mu-highlight-match-face' in the minibuffer."
  (let* ((query input)
         (_ (string-match "\\(?1:[a-zA-Z0-9\_\.\+\-]+@[a-zA-Z0-9\-]+\.[a-zA-Z0-9\-\.]+\\)" string))
         (email (match-string 1 string))
         (user (match-string 2 string))
         (domain (match-string 3 string))
         (extension (match-string 4 string))
         (name (string-trim (replace-regexp-in-string email "" string nil t nil nil)))
         (contact (list :name name :email email))
         (match-str (if (stringp input) (consult--split-escaped (car (consult--command-split query))) nil))
         (str (format "%s\s\s%s"
                      (propertize (consult-mu--set-string-width email (floor (* (frame-width) 0.55))) 'face 'consult-mu-sender-face)
                      (propertize name 'face 'consult-mu-subject-face)
                      ))
         (str (propertize str :contact contact :query query))
         )
    (if (and consult-mu-highlight-matches highlight)
        (cond
         ((listp match-str)
          (mapcar (lambda (match) (setq str (consult-mu--highlight-match match str t))) match-str))
         ((stringp match-str)
          (setq str (consult-mu--highlight-match match-str str t))))
      str)
    (cons str (list :contact contact :query query))))

(defun consult-mu-contacts--group-name (cand)
  "Gets the group name of CAND using `consult-mu-contacts-group-by'
See `consult-mu-contacts-group-by' for details of grouping options.
"
(let* ((contact (get-text-property 0 :contact cand))
       (email (plist-get contact :email))
       (name (plist-get contact :name))
       (_ (string-match "\\(?1:[a-zA-Z0-9\_\.\+\-]+\\)@\\(?2:[a-zA-Z0-9\-]+\.[a-zA-Z0-9\-\.]+\\)" email))
       (user (match-string 1 email))
       (domain (match-string 2 email))
       (group (or consult-mu-contacts--override-group consult-mu-contacts-group-by))
      (field (if (not (keywordp group)) (intern (concat ":" (format "%s" group))) group)))
      (pcase field
        (:email email)
        (:name (if (string-empty-p name) "n/a" name))
        (:domain domain)
        (:user user)
        (_ nil)
        )))

(defun consult-mu-contacts--group (cand transform)
"Group function for `consult-mu-contacts''s minibuffer candidates.

This is passed as GROUP to `consult--read' on candidates and is used to group contacts using `consult-mu-contacts--group-name'."
  (when-let ((name (consult-mu-contacts--group-name cand)))
    (if transform (substring cand) name)
    ))

(defun consult-mu-contacts--state ()
  "State function for `consult-mu-contacts' candidates.
This is passed as STATE to `consult--read' and is used to preview or do other actions on the candidate."
  (lambda (action cand)
    (let ((preview (consult--buffer-preview)))
      (pcase action
        ('preview
        ;;  (if cand
        ;;      (when-let* ((info (cdr cand))
        ;;                  (contact (plist-get info :contact))
        ;;                  (query (plist-get info :query))
        ;;                  (match-str (car (consult--command-split query)))
        ;;                  (mu4e-headers-buffer-name consult-mu-headers-buffer-name)
        ;;                  (buffer consult-mu-view-buffer-name))
        ;;        (add-to-list 'consult-mu--view-buffers-list buffer)
        ;;        (funcall preview action
        ;;                 (consult-mu--view msg t consult-mu-mark-previewed-as-read match-str)
        ;;                 )
        ;;        (with-current-buffer consult-mu-view-buffer-name
        ;;          (unless (one-window-p) (delete-other-windows))
        ;;          )))
        )
        ('return
         (save-mark-and-excursion
           (consult-mu--execute-all-marks)
           )
         (setq consult-mu-contacts--override-group nil)
         cand)
        ))))

(defun consult-mu-contacts--transform (async builder)
  "Adds annotation to minibuffer candiates for `consult-mu'.

Returns ASYNC function after formating results with `consult-mu-contacts--format-candidate'.
BUILDER is the command line builder function (e.g. `consult-mu-contacts--async-builder')."
  (let ((input))
    `(lambda (action)
       (cond
        ((stringp action)
         (setq input action)
         (funcall ,async action)
         )
        ((consp action)
         (funcall ,async (mapcar (lambda (string)
                      (consult-mu-contacts--format-candidate string input t))
                    action))
         )
         (t (funcall ,async action))
         )
         )))

(defun consult-mu-contacts--builder (input)
  "Build mu command line for searching messages by INPUT (e.g. `mu find INPUT)`."
  (pcase-let* ((consult-mu-args (append consult-mu-args '("cfind")))
               (cmd (consult--build-args consult-mu-args))
               (`(,arg . ,opts) (consult--command-split input))
               (flags (append cmd opts)))
    (unless (or (member "-n" flags) (member "--maxnum" flags))
      (if (and consult-mu-maxnum (> consult-mu-maxnum 0))
          (setq opts (append opts (list "--maxnum" (format "%s" consult-mu-maxnum))))))
    (if (or (member "-g" opts)  (member "--group" opts))
        (cond
         ((member "-g" opts)
          (setq consult-mu-contacts--override-group (ignore-errors (intern (nth (+ (cl-position "-g" opts :test 'equal) 1) opts))))
          (setq opts (remove "-g" (remove (ignore-errors (nth (+ (cl-position "-g" opts :test 'equal) 1) opts)) opts))))
         ((member "--group" opts)
          (setq consult-mu-contacts--override-group (ignore-errors (intern (nth (+ (cl-position "--group" opts :test 'equal) 1) opts))))
          (setq opts (remove "--group" (remove (ignore-errors (nth (+ (cl-position "--group" opts :test 'equal) 1) opts)) opts))))
         )
      (setq consult-mu-contacts--override-group nil)
      )
    (pcase-let* ((`(,re . ,hl) (funcall consult--regexp-compiler arg 'pcre t)))
      (when re
        (cons (append cmd
                      (list (string-join re " "))
                      opts)
              hl)))))

(defun consult-mu-contacts--async (prompt builder &optional initial)
"Query mu4e messages asynchronously.

This is a non-interactive internal function. For the interactive version see `consult-mu-contacts'.

It runs the command line from `consult-mu-contacts--builder' in an async process and returns the results (list of contacts) as a completion table in minibuffer that will be passed to `consult--read'. The completion table gets dynamically updated as the user types in the minibuffer. Each candidate in the minibuffer is formatted by `consult-mu-contacts--transform' to add annotation and other info to the candidate.

PROMPT is the prompt in the minibuffer (passed as PROMPT to `consult--red'.)
BUILDER is an async builder function passed to `consult--async-command'.
INITIAL is an optional arg for the initial input in the minibuffer. (passed as INITITAL to `consult--read'.)

commandline arguments/options (see `mu find --help` in the command line for details) can be passed to the minibuffer input similar to `consult-grep'. For example the user can enter:

`#john -- --maxnum 10'

this will search for contactss with the query \"john\", and retrives a maximum of 10 contacts.

Also, the results can further be narrowed by entering \"#\" similar to `consult-grep'.

For example:

`#john -- --maxnum 10#@gmail'

will retrieve the message as the example above, then narrows down the completion table to candidates that match \"@gmail\".
"
  (consult--read
   (consult--async-command builder
     (consult-mu-contacts--transform builder)
     )
   :prompt prompt
   ;;:lookup (consult-mu--lookup)
   :state (funcall #'consult-mu-contacts--state)
   :initial (consult--async-split-initial initial)
   :group #'consult-mu-contacts--group
   ;;:add-history (append (list (consult--async-split-thingatpt 'symbol))
   ;;                     consult-mu-saved-searches-dynamic
   ;;                     )
   ;;:history '(:input consult-mu--history)
   ;;:require-match t
   :category 'consult-mu-contacts
   :preview-key consult-mu-preview-key
   :sort t))

(defun consult-mu-contacts (&optional initial noaction)
    "Lists results of `mu find` Asynchronously.

This is an interactive wrapper function around `consult-mu-contacts--async'. It queries the user for a search term in the minibuffer, then fetches a list of messages for the entered search term as a minibuffer completion table for selection. The list of candidates in the completion table are dynamically updated as the user changes the entry.

Upon selection of a candidate either
 - the candidate is returned if NOACTION is non-nil
 or
 - the candidate is passed to `consult-mu-contacts-action' if NOACTION is nil.

Additional commandline arguments can be passed in the minibuffer entry by typing `--` followed by command line arguments.

For example the user can enter:

`#john doe -- -n 10'

this will run a contact earch with the query \"john doe\" and changes the search limit to 10.


Also, the results can further be narrowed by entering \"#\" similar to `consult-grep'.

For example:

`#john doe -- -n 10#@gmail'

will retrieve the message as the example above, then narrows down the completion table to candidates that match \"@gmail\".

INITIAL is an optional arg for the initial input in the minibuffer. (passed as INITITAL to `consult-mu-contacts--async').

For more details on consult--async functionalities, see `consult-grep' and the official manual of consult, here: https://github.com/minad/consult.
"
  (interactive)
  (save-mark-and-excursion
  (consult-mu--execute-all-marks)
  )
  (let* ((sel
        (consult-mu-contacts--async (concat "[" (propertize "consult-mu-contacts" 'face 'consult-mu-sender-face) "]" " Search Contacts:  ") #'consult-mu-contacts--builder initial)
         )
         )
    (save-mark-and-excursion
      (consult-mu--execute-all-marks)
      )
    (if noaction
        sel
      (progn
        (funcall consult-mu-contacts-action sel)
        sel))))

;;; provide `consult-mu-contacts' module
(provide 'consult-mu-contacts)

;;;  consult-mu-contacts.el ends here