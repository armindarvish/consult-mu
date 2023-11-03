;;; consult-mu-embark.el --- Emabrk Actions for consult-mu -*- lexical-binding: t -*-

;; Copyright (C) 2021-2023

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
(require 'embark)
(require 'consult-mu)


;;; Define Embark Action Functions
(defun consult-mu-embark-default-action (cand)
  "Open the link in an emacs buffer"
  (let* ((msgid (get-text-property 0 :msgid cand))
         (subject (get-text-property 0 :subject cand))
         (sender (get-text-property 0 :sender cand))
         (datetime (get-text-property 0 :datetime cand))
         (date (get-text-property 0 :date cand))
         (time (get-text-property 0 :time cand))
         (query (get-text-property 0 :query cand))
         (newcand (cons cand `(:msgid ,msgid :subject ,subject :sender ,sender :datetime ,datetime :date ,date :time ,time :query ,query))))
   (funcall consult-mu-action newcand)))

(defun consult-mu-embark-mark-for-refile (cand)
"Mark message for delete"
(let* ((msgid (get-text-property 0 :msgid cand))
        )
(consult-mu-headers-goto-message-id msgid)
(when-let ((buffer consult-mu-headers-buffer-name))
    (with-current-buffer buffer
      (display-buffer buffer)
      (mu4e-view-mark-for-refile)))))

(defun consult-mu-embark-refile (cand)
"Mark message for delete"
(let* ((msgid (get-text-property 0 :msgid cand))
       (msg (consult-mu--get-message-by-id msgid))
       (query (get-text-property 0 :query cand))
        )
(consult-mu-headers-goto-message-id msgid)
(when-let ((buffer consult-mu-headers-buffer-name))
    (with-current-buffer buffer
      (display-buffer buffer)
      (mu4e-headers-mark-for-refile)
      (mu4e-mark-execute-all)
      (mu4e~headers-update-handler msg t nil)
      (consult-mu--update-headers query nil msgid)
      ))))

;; (let ((msgid (get-text-property 0 :msgid cand)))
;;   (mu4e-view-message-with-message-id
;;    msgid)
;;   (with-current-buffer (mu4e-get-view-buffer)
;;   (mu4e-view-mark-for-mail-delete)
;;   )))

(defun consult-mu-embark-trash-message (cand)
  (let* ((msgid (get-text-property 0 :msgid cand))
         (msg (consult-mu--get-message-by-id msgid))
         (docid (mu4e-message-field msg :maildir))
        (maildir (mu4e-message-field msg :maildir))
        )
    ;;(mu4e-view-message-with-message-id msgid)
    )
)

;;; Define Embark Keymaps

(defvar-keymap consult-mu-embark-general-actions-map
  :doc "Keymap for consult-mu-embark"
  :parent embark-general-map
  "r" #'consult-mu-embark-refile
  "t" #'consult-mu-embark-trash-message
  )
(add-to-list 'embark-keymap-alist '(consult-mu . consult-mu-embark-general-actions-map))


(defvar-keymap consult-mu-embark-messages-actions-map
  :doc "Keymap for consult-mu-embark"
  :parent consult-mu-embark-general-actions-map
  )

(add-to-list 'embark-keymap-alist '(consult-mu-messages . consult-mu-embark-messages-actions-map))



;;(add-to-list 'embark-default-action-overrides '(consult-mu . consult-mu-embark-default-action))
(add-to-list 'embark-default-action-overrides '(consult-mu-messages . consult-mu-embark-default-action))

;;; Provide `consul-gh-embark' module

(provide 'consult-mu-embark)
