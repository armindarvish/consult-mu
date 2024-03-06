;;; consult-mu-embark.el --- Emabrk Actions for consult-mu -*- lexical-binding: t -*-

;; Copyright (C) 2021-2023

;; Author: Armin Darvish
;; Maintainer: Armin Darvish
;; Created: 2023
;; Version: 1.0
;; Package-Requires: ((emacs "28.0") (consult "0.34"))
;; Homepage: https://github.com/armindarvish/consult-mu
;; Keywords: convenience, matching, tools, email

;;; Commentary:

;;; Code:

;;; Requirements
(require 'embark)
(require 'consult-mu)

;;; Customization Variables
(defcustom consult-mu-embark-noconfirm-before-execute nil
  "Should consult-mu-embark skip confirmation when executing marks?"
  :group 'consult-mu
  :type 'boolean
  )

;;; Define Embark Action Functions
(defun consult-mu-embark-default-action (cand)
  "Run `consult-mu-action' on the candidate."
  (let* ((msg (get-text-property 0 :msg cand))
         (query (get-text-property 0 :query cand))
         (type (get-text-property 0 :type cand))
         (newcand (cons cand `(:msg ,msg :query ,query :type ,type))))
    (if (equal type :async)
        (consult-mu--update-headers query t msg :async)
      )
    (funcall consult-mu-action newcand))
  )


;; (defun consult-mu-embark-reply (cand)
;;   "Reply to message in CAND."
;;   (let* ((msg (get-text-property 0 :msg cand))
;;          (msgid (plist-get msg  :message-id))
;;          (query (get-text-property 0 :query cand))
;;          (buf (get-buffer consult-mu-headers-buffer-name))
;;          )
;;     (if buf
;;         (progn
;;           (with-current-buffer buf
;;             (if (eq major-mode 'mu4e-headers-mode)
;;                 (progn
;;                   (goto-char (point-min))
;;                   (mu4e-headers-goto-message-id msgid)
;;                   (if (equal (mu4e-message-field-at-point :message-id) msgid)
;;                       (mu4e-compose-reply)
;;                     (progn
;;                       (let* ((opts (cdr (consult--command-split query)))
;;                              (query (string-join (append (list (concat "msgid:" msgid) "--") opts) " ")))
;;                         (consult-mu--update-headers query t msgid))
;;                       (with-current-buffer buf
;;                         (goto-char (point-min))
;;                         (mu4e-headers-goto-message-id msgid)
;;                         (if (equal (mu4e-message-field-at-point :message-id) msgid)
;;                             (mu4e-compose-reply))))))
;;               (progn
;;                 (let* ((opts (cdr (consult--command-split query)))
;;                        (query (string-join (append (list (concat "msgid:" msgid) "--") opts) " ")))
;;                   (consult-mu--update-headers query t msgid))
;;                 (with-current-buffer buf
;;                   (goto-char (point-min))
;;                   (mu4e-headers-goto-message-id msgid)
;;                   (if (equal (mu4e-message-field-at-point :message-id) msgid)
;;                       (mu4e-compose-reply)))))
;;             )
;;           )
;;       (mu4e-update-index-nonlazy)
;;       )))

(defun consult-mu-embark-reply (cand)
  "Reply to message in CAND."
    (let* ((msg (get-text-property 0 :msg cand))
           (query (get-text-property 0 :query cand))
           (type (get-text-property 0 :type cand))
           (newcand (cons cand `(:msg ,msg :query ,query :type ,type)))
           (compose-buff nil))
      (if (equal type :async)
          (consult-mu--update-headers query t msg :async)
        )
      ;;(funcall consult-mu-action newcand)
      (with-current-buffer consult-mu-headers-buffer-name
        (unless (equal (mu4e-message-field-at-point :message-id) (plist-get msg :message-id))
          (mu4e-headers-goto-message-id))
        (if (equal (mu4e-message-field-at-point :message-id) (plist-get msg :message-id))
            (progn
              (setq compose-buff (mu4e-compose-reply))
              (mu4e~headers-update-handler msg nil nil)
              )
        ))

      (with-current-buffer consult-mu-view-buffer-name
        (quit-window)
        )
      (if (and compose-buff (buffer-live-p compose-buff))
          (progn (switch-to-buffer compose-buff)
                 (select-window (get-buffer-window compose-buff)))
        )
      ))

(defun consult-mu-embark-forward (cand)
  "Reply to message in CAND."
  (save-mark-and-excursion
    (let* ((msg (get-text-property 0 :msg cand))
           (query (get-text-property 0 :query cand))
           (type (get-text-property 0 :type cand))
           (newcand (cons cand `(:msg ,msg :query ,query :type ,type)))
           (compose-buff nil))
      (if (equal type :async)
          (consult-mu--update-headers query t msg :async)
        )
      ;;(funcall consult-mu-action newcand)
      (with-current-buffer consult-mu-headers-buffer-name
        (unless (equal (mu4e-message-field-at-point :message-id) (plist-get msg :message-id))
          (mu4e-headers-goto-message-id))
        (if (equal (mu4e-message-field-at-point :message-id) (plist-get msg :message-id))
            (progn
              (setq compose-buff (mu4e-compose-forward))
              (mu4e~headers-update-handler msg nil nil)
              )
        ))

      (with-current-buffer consult-mu-view-buffer-name
        (quit-window)
        )
      (if (and compose-buff (buffer-live-p compose-buff))
          (progn (switch-to-buffer compose-buff)
                 (select-window (get-buffer-window compose-buff)))
        )
      )))

(defun consult-mu-embark-kill-message-field (cand)
  "Get a header field of message in CAND."
    (let* ((msg (get-text-property 0 :msg cand))
           (query (get-text-property 0 :query cand))
           (type (get-text-property 0 :type cand))
           (newcand (cons cand `(:msg ,msg :query ,query :type ,type)))
           )
      (if (equal type :async)
          (consult-mu--update-headers query t msg :async)
        )
      (with-current-buffer consult-mu-headers-buffer-name
        (unless (equal (mu4e-message-field-at-point :message-id) (plist-get msg :message-id))
          (mu4e-headers-goto-message-id))
        (if (equal (mu4e-message-field-at-point :message-id) (plist-get msg :message-id))
            (progn
              (mu4e~headers-update-handler msg nil nil)
              )
        ))

      (with-current-buffer consult-mu-view-buffer-name
        (kill-new (consult-mu--message-get-header-field))
        (consult-mu--pulse-region (point) (point-at-eol))
        )
      ))

(defun consult-mu-embark-save-attachmnts (cand)
  "Save attachments of CAND."
    (let* ((msg (get-text-property 0 :msg cand))
           (query (get-text-property 0 :query cand))
           (type (get-text-property 0 :type cand))
           (newcand (cons cand `(:msg ,msg :query ,query :type ,type)))
           )
      (if (equal type :async)
          (consult-mu--update-headers query t msg :async)
        )
      (with-current-buffer consult-mu-headers-buffer-name
        (unless (equal (mu4e-message-field-at-point :message-id) (plist-get msg :message-id))
          (mu4e-headers-goto-message-id))
        (if (equal (mu4e-message-field-at-point :message-id) (plist-get msg :message-id))
            (progn
              (mu4e~headers-update-handler msg nil nil)
              )
        ))

      (with-current-buffer consult-mu-view-buffer-name
        (goto-char (point-min))
        (re-search-forward "^\\(Attachment\\|Attachments\\): ")
        (consult-mu--pulse-region (point) (point-at-eol))
        (mu4e-view-save-attachments t)
        )
      ))

(defun consult-mu-embark-search-messages-from-contact (cand)
  "Searh messages from the same contact as the message in CAND."
    (let* ((msg (get-text-property 0 :msg cand))
           (from (car (plist-get msg :from)))
           (email (plist-get from :email))
           )
      (consult-mu (concat "from:" email)))
      )

(defun consult-mu-embark-search-messages-with-subject (cand)
  "Searh messages from the same contact as the message in CAND."
    (let* ((msg (get-text-property 0 :msg cand))
           (subject (plist-get msg :subject))
           )
      (consult-mu (concat "subject:" subject)))
      )


;; (cl-letf* (((symbol-function #'mu4e~headers-append-handler) #'consult-mu--headers-append-handler)
;;            ((symbol-function #'mu4e-view) #'consult-mu--view-msg)
;;            ((symbol-function #'mu4e~set-sent-handler-message-sent-hook-fn) (lambda ())))
;;   (save-excursion
;;     (let* ((msg (get-text-property 0 :msg cand))
;;            (msgid (plist-get msg  :message-id))
;;            (query (get-text-property 0 :query cand))
;;            (buf (get-buffer consult-mu-headers-buffer-name))
;;            )
;;       (if buf
;;           (prog
;;             (with-current-buffer buf
;;               (if (eq major-mode 'mu4e-headers-mode)
;;                   (progn
;;                     (goto-char (point-min))
;;                     (mu4e-headers-goto-message-id msgid)
;;                     (if (equal (mu4e-message-field-at-point :message-id) msgid)
;;                         (mu4e-compose-forward)
;;                       (progn
;;                         (let* ((opts (cdr (consult--command-split query)))
;;                                (query (string-join (append (list (concat "msgid:" msgid) "--") opts) " ")))
;;                           (consult-mu--update-headers query t msgid))
;;                         (with-current-buffer buf
;;                           (goto-char (point-min))
;;                           (mu4e-headers-goto-message-id msgid)
;;                           (if (equal (mu4e-message-field-at-point :message-id) msgid)
;;                               (mu4e-compose-forward))))))
;;                 (progn
;;                   (let* ((opts (cdr (consult--command-split query)))
;;                          (query (string-join (append (list (concat "msgid:" msgid) "--") opts) " ")))
;;                     (consult-mu--update-headers query t msgid))
;;                   (with-current-buffer buf
;;                     (goto-char (point-min))
;;                     (mu4e-headers-goto-message-id msgid)
;;                     (if (equal (mu4e-message-field-at-point :message-id) msgid)
;;                         (mu4e-compose-forward)))))
;;               )

;;             )
;;         )))))

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
                                  (mu4e-headers-mark-and-next ',mark)))))
                        )
                      )
                  )

                )))))

;; use consult-mu-embark--defun-func-for-marks to make a function for each `mu4e-marks' element.
(consult-mu-embark--defun-func-for-marks (mapcar 'car mu4e-marks))

;; add embark functions for marks
(defun consult-mu-embark--defun-func-for-marks (marks)
  "Runs the macro `consult-mu-embark--defun-mark-for' on a list of marks.

This is useful for creating embark functions for all the `mu4e-marks' elements."
  (mapcar (lambda (mark) (eval `(consult-mu-embark--defun-mark-for ,mark))) marks))

;; use consult-mu-embark--defun-func-for-marks to make a function for each `mu4e-marks' element.
(consult-mu-embark--defun-func-for-marks (mapcar 'car mu4e-marks))

;;; Define Embark Keymaps
(defvar-keymap consult-mu-embark-general-actions-map
  :doc "Keymap for consult-mu-embark"
  :parent embark-general-map
  )

(add-to-list 'embark-keymap-alist '(consult-mu . consult-mu-embark-general-actions-map))


(defvar-keymap consult-mu-embark-messages-actions-map
  :doc "Keymap for consult-mu-embark-messages"
  :parent consult-mu-embark-general-actions-map
  "r" #'consult-mu-embark-reply
  "f" #'consult-mu-embark-forward
  "?" #'consult-mu-embark-kill-message-field
  "c" #'consult-mu-embark-search-messages-from-contact
  "s" #'consult-mu-embark-search-messages-with-subject
  "S" #'consult-mu-embark-save-attachmnts
  )

(add-to-list 'embark-keymap-alist '(consult-mu-messages . consult-mu-embark-messages-actions-map))


;; add mark keys to `consult-mu-embark-messages-actions-map' keymap
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

;; change the default action on `consult-mu-messages' category.
(add-to-list 'embark-default-action-overrides '(consult-mu-messages . consult-mu-embark-default-action))


;;; Provide `consult-mu-embark' module

(provide 'consult-mu-embark)

;;;  consult-mu-embark.el ends here
