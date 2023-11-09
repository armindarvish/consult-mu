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

;;; Customization Variables
(defcustom consult-mu-embark-noconfirm-before-exxecute nil
  "Should consult-mu-embark skip confirmation when executing marks?"
  :group 'consult-mu
  :type 'boolean
  )

;;; Define Embark Action Functions
(defun consult-mu-embark-default-action (cand)
  "Open the link in an emacs buffer"
  (let* ((msg (get-text-property 0 :msg cand))
         (query (get-text-property 0 :query cand))
         (newcand (cons cand `(:msg ,msg :query ,query))))
    (funcall consult-mu-action newcand)
    ))

;;; Define Embark Keymaps

(defvar-keymap consult-mu-embark-general-actions-map
  :doc "Keymap for consult-mu-embark"
  :parent embark-general-map
  )

(add-to-list 'embark-keymap-alist '(consult-mu . consult-mu-embark-general-actions-map))


(defvar-keymap consult-mu-embark-messages-actions-map
  :doc "Keymap for consult-mu-embark"
  :parent consult-mu-embark-general-actions-map
  )
(add-to-list 'embark-keymap-alist '(consult-mu-messages . consult-mu-embark-messages-actions-map))

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
                     (buf (get-buffer consult-mu-headers-buffer-name))
                     )
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
                                  (let* ((opts (cdr (consult--command-split query)))
                                         (query (string-join (append (list (concat "msgid:" msgid) "--") opts) " ")))
                                    (consult-mu--update-headers query t msgid))
                                  (with-current-buffer buf
                                    (goto-char (point-min))
                                    (mu4e-headers-goto-message-id msgid)
                                    (if (equal (mu4e-message-field-at-point :message-id) msgid)
                                        (mu4e-headers-mark-and-next ',mark))))))
                          (progn
                            (let* ((opts (cdr (consult--command-split query)))
                                   (query (string-join (append (list (concat "msgid:" msgid) "--") opts) " ")))
                              (consult-mu--update-headers query t msgid))
                            (with-current-buffer buf
                              (goto-char (point-min))
                              (mu4e-headers-goto-message-id msgid)
                              (if (equal (mu4e-message-field-at-point :message-id) msgid)
                                  (mu4e-headers-mark-and-next ',mark)))))
                        )
                      )
                  )

                )))))

;; add embark functions for marks
(defun consult-mu-embark--defun-func-for-marks (marks)
  (mapcar (lambda (mark) (eval `(consult-mu-embark--defun-mark-for ,mark))) marks))

(consult-mu-embark--defun-func-for-marks (mapcar 'car mu4e-marks))

;; add mark keys to keymap
(defun consult-mu-embark--add-keys-for-marks (marks)
  (mapcar (lambda (mark)
            (let* ((key (plist-get (cdr mark) :char))
                   (key (cond ((consp key) (car key)) ((stringp key) key)))
                   (func (intern (concat "consult-mu-embark-mark-for-" (format "%s" (car mark)))))
                   (key (concat "m" key)))
              (define-key consult-mu-embark-messages-actions-map key func)
              ))
          marks))

(consult-mu-embark--add-keys-for-marks mu4e-marks)

;; change the default action
(add-to-list 'embark-default-action-overrides '(consult-mu-messages . consult-mu-embark-default-action))


;;; Provide `consul-gh-embark' module

(provide 'consult-mu-embark)
