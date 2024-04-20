;;; mono-complete-backend-spell-fu.el --- Spell-FU back-end -*- lexical-binding: t -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2023  Campbell Barton
;; URL: https://codeberg.org/ideasman42/emacs-mono-complete
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;; Spell-FU backend (complete based on Spell-FU dictionary).
;
;;; Code:

(require 'mono-complete)

;; Note: uses `spell-fu' variables:
;; - `spell-fu-word-regexp'.
;; - `spell-fu-dictionaries'.

;; ---------------------------------------------------------------------------
;; Internal Utilities

(defun mono-complete-backend-spell-fu--simple-capitalize-word (word)
  "An alternative to `capitalize' WORD.

This only ever make the first letter upper-case."
  (concat (upcase (substring word 0 1)) (downcase (substring word 1))))

;; ---------------------------------------------------------------------------
;; Callback Implementations

(defun mono-complete-backend-spell-fu-prefix ()
  "Return the prefix at point."
  (let ((result nil)
        (pos (point)))

    (save-excursion
      ;; If there is blank space before the cursor, don't look further.
      (when (zerop (skip-chars-backward "[:blank:]" (1- pos)))
        (let ((word-regexp (bound-and-true-p spell-fu-word-regexp))
              (pos-bol (line-beginning-position)))

          (save-restriction
            (save-match-data
              ;; Narrow and widen because the search start position causes `spell-fu-word-regexp'
              ;; not to match when the cursor is in the middle of a word.
              (narrow-to-region pos-bol pos)
              (when (re-search-backward word-regexp pos-bol t)
                (widen)
                ;; Allow matching past 'pos'.
                (when (looking-at word-regexp)
                  (let ((beg (match-beginning 0))
                        (end (match-end 0)))
                    (when (eq end pos)
                      (setq result (buffer-substring-no-properties beg end)))))))))))
    result))


(defun mono-complete-backend-spell-fu-complete (_config prefix cache)
  "Complete at point based on PREFIX & CACHE."
  (let ((result nil) ; Return a list of strings or nil.
        (result-length 0)

        (prefix-length (length prefix))
        (prefix-lower (downcase prefix))
        (prefix-upper (upcase prefix))
        (prefix-title (mono-complete-backend-spell-fu--simple-capitalize-word prefix)))

    (let ((test-fn
           (cond
            ;; Lower.
            ((string-equal prefix prefix-lower)
             (lambda (k _v)
               (when (string-prefix-p prefix-lower k)
                 (let ((k-length (length k)))
                   (when (or (null result) (< k-length result-length))
                     (unless (eq prefix-length k-length)
                       (setq result-length k-length)
                       (setq result k)))))))
            ;; Capitalize (also captures single characters as it should).
            ((string-equal prefix prefix-title)
             (lambda (k _v)
               (when (string-prefix-p prefix-lower k)
                 (let ((k-length (length k)))
                   (when (or (null result) (< k-length result-length))
                     (unless (eq prefix-length k-length)
                       (setq result-length k-length)
                       (setq result
                             (mono-complete-backend-spell-fu--simple-capitalize-word k))))))))
            ;; Upper.
            ((string-equal prefix prefix-upper)
             (lambda (k _v)
               (when (string-prefix-p prefix-lower k)
                 (let ((k-length (length k)))
                   (when (or (null result) (< k-length result-length))
                     (unless (eq prefix-length k-length)
                       (setq result-length k-length)
                       (setq result (upcase k))))))))
            ;; Mixed case (just append).
            (t
             (lambda (k _v)
               (when (string-prefix-p prefix-lower k)
                 (let ((k-length (length k)))
                   (when (or (null result) (< k-length result-length))
                     (unless (eq prefix-length k-length)
                       (setq result-length k-length)
                       (setq result (concat prefix (substring k prefix-length))))))))))))

      (dolist (dict (bound-and-true-p spell-fu-dictionaries))
        (maphash test-fn (symbol-value dict))))

    (when result
      (setq result (list (substring result prefix-length))))

    (cons result cache)))


;; ---------------------------------------------------------------------------
;; Public Callback

;;;###autoload
(defun mono-complete-backend-spell-fu ()
  "DEBBREV completer."
  (list
   :prefix #'mono-complete-backend-spell-fu-prefix
   :complete #'mono-complete-backend-spell-fu-complete))

(provide 'mono-complete-backend-spell-fu)
;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; End:
;;; mono-complete-backend-spell-fu.el ends here
