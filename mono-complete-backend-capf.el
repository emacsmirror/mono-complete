;;; mono-complete-backend-capf.el --- DABBREV back-end -*- lexical-binding: t -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2023  Campbell Barton
;; URL: https://codeberg.org/ideasman42/emacs-mono-complete
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;; Completion at point function (capf) back-end.

;;; Code:

(require 'mono-complete)


;; ---------------------------------------------------------------------------
;; Custom Variables

(defgroup mono-complete-backend-capf nil
  "Word prediction for mono-complete, generated on demand."
  :group 'convenience)

(defcustom mono-complete-backend-capf-complete-fn nil
  "The completion function.
When nil, defaults are used (depending on the mode)."
  :type '(choice (const nil) function))


;; ---------------------------------------------------------------------------
;; Callbacks

(defun mono-complete-backend-capf-setup (config)
  "Validate CONFIG."
  (let ((complete-fn (plist-get config :complete-fn)))

    ;; Find a completion function.
    (unless complete-fn
      (cond
       ((and (fboundp 'eglot-managed-p) (funcall #'eglot-managed-p))
        (setq config (plist-put config :complete-fn 'eglot-completion-at-point)))
       ((eq major-mode 'emacs-lisp-mode)
        (setq config (plist-put config :complete-fn 'elisp-completion-at-point)))
       (t ; Disable this backend if no functions can be found.
        (setq config t)))))

  config)

(defun mono-complete-backend-capf-prefix ()
  "Return the prefix at point."
  (save-excursion
    (let ((pos-init (point)))
      (back-to-indentation)
      (cond
       ((< (point) pos-init)
        (buffer-substring-no-properties pos-init (point)))
       (t
        nil)))))

(defun mono-complete-backend-capf-complete (config _prefix cache)
  "Complete at point based on CONFIG, PREFIX & CACHE."
  ;; Return a list of strings or nil.
  (let ((result nil))
    (let ((comp-data (funcall (plist-get config :complete-fn))))
      (when comp-data
        (pcase-let ((`(,beg ,end ,collection) comp-data))
          (completion-in-region beg end collection
                                (lambda (&rest x)
                                  ;; For now just use the first hit.
                                  ;; It's possible to select the shortest or something else.
                                  (unless result
                                    (let ((complete (car x)))
                                      ;; May be a string or a symbol.
                                      (when (symbolp complete)
                                        (setq complete (symbol-name complete)))
                                      (setq result
                                            (substring-no-properties complete (- end beg)))))
                                  nil)))))

    (when result
      (setq result (list result)))

    (cons result cache)))


;; ---------------------------------------------------------------------------
;; Public Callback

;;;###autoload
(defun mono-complete-backend-capf ()
  "Wrapper for `completion-at-point' using CONFIG."
  (list
   :setup #'mono-complete-backend-capf-setup
   :prefix #'mono-complete-backend-capf-prefix
   :complete #'mono-complete-backend-capf-complete))

(provide 'mono-complete-backend-capf)
;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; End:
;;; mono-complete-backend-capf.el ends here
