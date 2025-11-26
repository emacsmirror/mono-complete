;;; mono-complete-backend-dabbrev.el --- DABBREV back-end -*- lexical-binding: t -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2023  Campbell Barton
;; URL: https://codeberg.org/ideasman42/emacs-mono-complete
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;; DABBREV back-end.

;;; Code:

(require 'dabbrev)

;; ---------------------------------------------------------------------------
;; Internal Utilities

(defmacro mono-complete-backend-dabbrev--with-advice (advice &rest body)
  "Execute BODY with ADVICE temporarily enabled.

Advice are triplets of (SYMBOL HOW FUNCTION),
see `advice-add' documentation."
  (declare (indent 1))
  (let ((advice-list advice)
        (body-let nil)
        (body-advice-add nil)
        (body-advice-remove nil)
        (item nil))
    (unless (listp advice-list)
      (error "Advice must be a list"))
    (cond
     ((null advice-list)
      (macroexp-warn-and-return
       "An empty advice argument was found"
       `(progn
          ,@body)))
     (t
      (while (setq item (pop advice-list))
        (unless (and (listp item) (eq 3 (length item)))
          (error "Each advice must be a list of 3 items"))
        (let ((fn-sym (gensym))
              (fn-advise (pop item))
              (fn-advice-ty (pop item))
              (fn-body (pop item)))
          ;; Build the calls for each type.
          (push (list fn-sym fn-body) body-let)
          (push (list 'advice-add fn-advise fn-advice-ty fn-sym) body-advice-add)
          (push (list 'advice-remove fn-advise fn-sym) body-advice-remove)))
      (setq body-let (nreverse body-let))
      (setq body-advice-add (nreverse body-advice-add))

      ;; Compose the call.
      `(let ,body-let
         (unwind-protect
             (progn
               ,@body-advice-add
               ,@body)
           ,@body-advice-remove))))))

(defmacro mono-complete-backend-dabbrev--with-suppressed-message (&rest body)
  "Run BODY with the message function disabled entirely."
  (declare (indent 0))
  `(mono-complete-backend-dabbrev--with-advice (('message :override (lambda (&rest _args) nil)))
     ,@body))

(defmacro mono-complete-backend-dabbrev--with-suppressed-message-advice (function-sym &rest body)
  "Advise FUNCTION-SYM to run BODY with `message' disabled."
  (declare (indent 1))
  `(mono-complete-backend-dabbrev--with-advice
       ((,function-sym
         :around
         (lambda (fn-orig &rest args)
           (mono-complete-backend-dabbrev--with-suppressed-message
             (apply fn-orig args)))))
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
          (while (and (looking-at dabbrev--abbrev-char-regexp) (null (eq pos-next (point))))
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
