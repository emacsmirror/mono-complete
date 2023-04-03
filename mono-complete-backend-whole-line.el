;;; mono-complete-backend-whole-line.el --- DABBREV back-end -*- lexical-binding: t -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2023  Campbell Barton
;; URL: https://codeberg.org/ideasman42/emacs-mono-complete
;; Version: 0.1

;;; Commentary:
;; Whole line in buffer back-end.
;
;;; Code:

(require 'mono-complete)

(defgroup mono-complete-backend-whole-line nil
  "Whole line prediction for mono-complete, generated on demand."
  :group 'convenience)

(defcustom mono-complete-backend-whole-line-trailing-text 'match
  "Handling of trailing text (after the cursor).

- \"Match Trailing\" completion lines must match text after the cursor.
  This text is trimmed from the completion suggestion.
- \"No Trailing\" completion is only performed when there is no trailing text.
- \"Ignore Trailing\" completion ignores text after the cursor."
  :type
  '(choice (const :tag "Match Trailing" match)
           (const :tag "No Trailing" none)
           (const :tag "Ignore Trailing" ignore)))


;; ---------------------------------------------------------------------------
;; Internal Utilities

(defun mono-complete-backend-whole-line--tailing-text-as-string (pos)
  "Return the text trailing POS to the end of line (excluding trailing blank)."
  (save-excursion
    (end-of-line)
    (skip-chars-backward "[:blank:]" pos)
    (cond
     ((< pos (point))
      (buffer-substring-no-properties pos (point)))
     (t
      nil))))

(defun mono-complete-backend-whole-line--tailing-text-p (pos)
  "Return non-nil when there is trailing (non-blank) text from POS."
  (save-excursion
    (end-of-line)
    (skip-chars-backward "[:blank:]" pos)
    (< pos (point))))


;; ---------------------------------------------------------------------------
;; Callbacks

(defun mono-complete-backend-whole-line-prefix ()
  "Return the prefix at point."
  (save-excursion
    (let ((pos-init (point)))
      (cond
       ((and (eq mono-complete-backend-whole-line-trailing-text 'none)
             (mono-complete-backend-whole-line--tailing-text-p pos-init))
        ;; Any trailing non-blank means complete is disabled.
        nil)
       (t
        (back-to-indentation)
        (cond
         ((< (point) pos-init)
          (buffer-substring-no-properties pos-init (point)))
         (t
          nil)))))))

(defun mono-complete-backend-whole-line-complete (_config prefix cache)
  "Complete at point based on PREFIX & CACHE."
  (let ((result nil) ; Return a list of strings or nil.
        (search-re (concat "^[[:blank:]]*" "\\(" (regexp-quote prefix) "\\)"))
        (pos-init (point))
        (trailing-text
         (cond
          ((eq mono-complete-backend-whole-line-trailing-text 'match)
           (mono-complete-backend-whole-line--tailing-text-as-string (point)))
          (t
           nil))))

    (cond
     (trailing-text
      (setq search-re (concat search-re "[^\n]*" (regexp-quote trailing-text))))
     (t
      ;; At least one character, avoids matching many lines with the same prefix and no suffix.
      ;; Without this, a HR literal "---" can hang on auto-completion when there are
      ;; many other HR's in the file.
      (setq search-re (concat search-re "[^\n]"))))

    (save-match-data
      (save-excursion

        ;; Search backwards.
        (beginning-of-line)
        (while (and (null result) (re-search-backward search-re nil t))
          (let ((pos-beg (match-end 1)))
            (end-of-line)
            (skip-chars-backward "[:blank:]" pos-beg)
            (let ((pos-end (point)))
              (when trailing-text
                (setq pos-end (- pos-end (length trailing-text))))
              (when (< pos-beg (point))
                (setq result (list (buffer-substring-no-properties pos-beg pos-end)))))))

        (unless result
          (goto-char pos-init)

          ;; Search forwards.
          (end-of-line)
          (while (and (null result) (re-search-forward search-re nil t))
            (let ((pos-beg (match-end 1)))
              (end-of-line)
              (skip-chars-backward "[:blank:]" pos-beg)
              (let ((pos-end (point)))
                (when trailing-text
                  (setq pos-end (- pos-end (length trailing-text))))
                (when (< pos-beg pos-end)
                  (setq result (list (buffer-substring-no-properties pos-beg pos-end))))))))))
    (cons result cache)))


;; ---------------------------------------------------------------------------
;; Public Callback

;;;###autoload
(defun mono-complete-backend-whole-line ()
  "DEBBREV completer."
  (list
   :prefix #'mono-complete-backend-whole-line-prefix
   :complete #'mono-complete-backend-whole-line-complete))

(provide 'mono-complete-backend-whole-line)
;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; elisp-autofmt-format-quoted: nil
;; End:
;;; mono-complete-backend-whole-line.el ends here
