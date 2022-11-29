;;; mono-complete-backend-dabbrev.el --- DABBREV back-end -*- lexical-binding: t -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2023  Campbell Barton
;; URL: https://codeberg.org/ideasman42/emacs-mono-complete
;; Version: 0.1

;;; Commentary:
;; DABBREV back-end.

;;; Code:

(require 'dabbrev)

;; ---------------------------------------------------------------------------
;; Internal Utilities

(defmacro mono-complete-backend-dabbrev--with-advice (fn-orig where fn-advice &rest body)
  "Execute BODY with advice.
Added WHERE using FN-ADVICE temporarily added to FN-ORIG."
  (declare (indent 3))
  `(let ((fn-advice-var ,fn-advice))
     (unwind-protect
         (progn
           (advice-add ,fn-orig ,where fn-advice-var)
           ,@body)
       (advice-remove ,fn-orig fn-advice-var))))

(defmacro mono-complete-backend-dabbrev--with-suppressed-message (&rest body)
  "Run BODY with the message function disabled entirely."
  (declare (indent 0))
  `(mono-complete-backend-dabbrev--with-advice 'message :override (lambda (&rest _args) nil)
     ,@body))

(defmacro mono-complete-backend-dabbrev--with-suppressed-message-advice (function-sym &rest body)
  "Advise FUNCTION-SYM to run BODY with `message' disabled."
  (declare (indent 1))
  `(mono-complete-backend-dabbrev--with-advice ,function-sym
       :around
       (lambda (fn-orig &rest args)
         (mono-complete-backend-dabbrev--with-suppressed-message
           (apply fn-orig args)))
     ,@body))

;; Messages from `dabbrev--find-expansion' ("Scanning for dabbrevs...done")
;; are suppressed since they are annoying when searching for a candidate
;; for the preview.
(defmacro mono-complete-backend-dabbrev--without-progress-reporter (&rest body)
  "Run BODY with the progress reporter with `message' disabled."
  (declare (indent 0))
  `(mono-complete-backend-dabbrev--with-suppressed-message-advice 'make-progress-reporter
     (mono-complete-backend-dabbrev--with-suppressed-message-advice 'progress-reporter-update
       (mono-complete-backend-dabbrev--with-suppressed-message-advice 'progress-reporter-done
         ,@body))))


;; ---------------------------------------------------------------------------
;; Internal DABBREV Wrappers

(defun mono-complete-backend-dabbrev--get-expansion (prefix)
  "Get expansion for PREFIX."
  ;; Messages from `dabbrev--find-expansion' ("Scanning for dabbrevs...done")
  ;; are suppressed since they are annoying when searching for a candidate
  ;; for the preview.
  (mono-complete-backend-dabbrev--without-progress-reporter
    (let ((expansion (dabbrev--find-expansion prefix 0 dabbrev-case-fold-search)))
      (when expansion

        (cond
         ;; NOTE: we might want to use this if we ever want DABBREV
         ;; to be able to manipulate the case of the prefix.
         (nil
          (with-temp-buffer
            (insert prefix)
            (let ((pos-init (point)))
              (dabbrev--substitute-expansion prefix prefix expansion nil)
              (buffer-substring-no-properties pos-init (point-max)))))
         (t
          ;; Works in some cases but not all.
          (substring-no-properties expansion (length prefix) (length expansion))))))))


;; ---------------------------------------------------------------------------
;; Callback Implementations

(defun mono-complete-backend-dabbrev-prefix ()
  "Return the prefix at point."
  (let ((ch (char-before)))
    (cond
     ;; Early exit on space character.
     ;;
     ;; The nil case accounts for being at the beginning of the buffer.
     ;; Unlikely as this typically runs after text insertion,
     ;; nevertheless, account for this case.
     ((memq ch (list ?\s ?\t ?\n nil))
      ;; Skip.
      nil)
     (t
      (save-excursion
        (unless dabbrev--abbrev-char-regexp
          (dabbrev--reset-global-variables))
        (let ((pos-init (point))
              (pos-next nil))
          (forward-char -1)
          (while (and (looking-at dabbrev--abbrev-char-regexp) (not (eq pos-next (point))))
            (setq pos-next (point))
            (unless (bobp)
              (forward-char -1)))
          (when (and pos-next (< pos-next pos-init))
            (buffer-substring-no-properties pos-next pos-init))))))))

(defun mono-complete-backend-dabbrev-complete (_config prefix cache)
  "Complete at point based on PREFIX & CACHE."
  ;; Only reset on first run.
  (unless cache
    (dabbrev--reset-global-variables)
    (setq cache t))

  (let ((expansion-suffix (mono-complete-backend-dabbrev--get-expansion prefix)))
    (cons
     (cond
      (expansion-suffix
       (list expansion-suffix))
      (t
       nil))
     cache)))


;; ---------------------------------------------------------------------------
;; Public Callback

;;;###autoload
(defun mono-complete-backend-dabbrev ()
  "DEBBREV completer."
  (list
   :prefix #'mono-complete-backend-dabbrev-prefix
   :complete #'mono-complete-backend-dabbrev-complete))

(provide 'mono-complete-backend-dabbrev)
;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; End:
;;; mono-complete-backend-dabbrev.el ends here
