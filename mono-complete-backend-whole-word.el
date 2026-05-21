;;; mono-complete-backend-whole-word.el --- Whole word back-end -*- lexical-binding: t -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2023  Campbell Barton
;; URL: https://codeberg.org/ideasman42/emacs-mono-complete
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;; Whole word in buffer back-end.
;
;;; Code:

(require 'mono-complete)

(defgroup mono-complete-backend-whole-word nil
  "Whole word prediction for mono-complete, generated on demand."
  :group 'convenience)

(defcustom mono-complete-backend-whole-word-type 'symbol
  "How word boundaries are selected.

- \"Word\" complete words.
- \"Symbol\" complete symbols."
  :type
  '(choice (const :tag "Word" word)
           (const :tag "Symbol" symbol)))

(defcustom mono-complete-backend-whole-word-trailing-text 'match
  "Handling of trailing text (after the cursor).

- \"Match Trailing\" completion words must match text after the cursor.
  This text is trimmed from the completion suggestion.
- \"No Trailing\" completion is only performed when there is no trailing text.
- \"Ignore Trailing\" completion ignores text after the cursor."
  :type
  '(choice (const :tag "Match Trailing" match)
           (const :tag "No Trailing" none)
           (const :tag "Ignore Trailing" ignore)))

;; ---------------------------------------------------------------------------
;; Internal Utilities

(defun mono-complete-backend-whole-word--bounds ()
  (cond
   ((eq mono-complete-backend-whole-word-type 'symbol)
    (bounds-of-thing-at-point 'symbol))
   (t
    (bounds-of-thing-at-point 'word))))

(defun mono-complete-backend-whole-word--tailing-text-as-string (pos)
  "Return the text trailing POS to the end of word or symbol."
  (let ((pos-end (cdr-safe (mono-complete-backend-whole-word--bounds))))
    (cond
     ((and pos-end (< (point) pos-end))
      (buffer-substring-no-properties (point) pos-end))
     (t
      nil))))

(defun mono-complete-backend-whole-word--tailing-text-p (pos)
  "Return non-nil when there is trailing (non-blank) text from POS."
  (save-excursion
    (end-of-line)
    (skip-chars-backward "[:blank:]" pos)
    (< pos (point))))

(defsubst mono-complete-backend-whole-word--trim-prefix-or-nil (prefix w)
  (let ((prefix-length (length prefix))
        (w-length (length w)))
    (cond
     ;; For the purpose of picking matches to suggest,
     ;; if they are equal, the word should be ignored.
     ((>= prefix-length w-length)
      nil)
     ((equal prefix (substring w 0 prefix-length))
      (substring w prefix-length)))))

(defsubst mono-complete-backend-whole-word--trim-suffix-or-nil (suffix w)
  (let ((suffix-length (length suffix))
        (w-length (length w)))
    (cond
     ;; For the purpose of picking matches to suggest,
     ;; if they are equal, the word should be ignored.
     ((>= suffix-length w-length)
      nil)
     ((equal suffix (substring w (- w-length suffix-length)))
      (substring w 0 (- w-length suffix-length))))))


;; ---------------------------------------------------------------------------
;; Callbacks

(defun mono-complete-backend-whole-word-prefix ()
  "Return the prefix at point."
  (let ((pos-init (point))
        (pos-beg (car-safe (mono-complete-backend-whole-word--bounds))))
    (cond
     ((and (eq mono-complete-backend-whole-word-trailing-text 'none)
           (mono-complete-backend-whole-word--tailing-text-p pos-init))
      ;; Any trailing non-blank means complete is disabled.
      nil)
     ((and pos-beg (< pos-beg pos-init))
      (buffer-substring-no-properties pos-beg pos-init))
     (t
      nil))))

(defun mono-complete-backend-whole-word--complete-impl (prefix cache trailing-text)
  "Complete at point based on PREFIX & CACHE."
  (let ((result nil) ; Return a list of strings or nil.
        (search-re
         (cond
          ((eq mono-complete-backend-whole-word-type 'symbol)
           "\\_<\\(\\sw\\|\\s_\\)+\\_>")
          (t
           "\\b\\(\\sw\\)+\\b")))

        (pos-init (point)))

    ;; Note that symbol is the:
    ;; (path . symbols)
    (unless cache
      ;; Always overwrite next.
      (let ((words nil))

        (save-match-data
          (save-excursion

            ;; Search backwards.
            (beginning-of-line)
            (while (re-search-backward search-re nil t)
              (push (match-string-no-properties 0) words))

            (goto-char pos-init)
            (beginning-of-line)

            (while (re-search-forward search-re nil t)
              (push (match-string-no-properties 0) words))))

        (setq cache (cons nil words))))

    ;; Now we have the words, match with the prefix.
    (let ((words (cdr cache)))
      (while (and (null result) words)
        (let ((w
               (prog1 (car words)
                 (setq words (cdr words)))))
          (let ((w-test (mono-complete-backend-whole-word--trim-prefix-or-nil prefix w)))
            (when w-test
              (when trailing-text
                (setq w-test
                      (mono-complete-backend-whole-word--trim-suffix-or-nil trailing-text w-test)))
              (when w-test
                (setq result (list w-test))))))))

    (unless result
      ;; If nothing is found, include partial matches.
      (let ((words (cdr cache))
            (prefix-as-regex (regexp-quote prefix)))
        (save-match-data
          (while (and (null result) words)
            (let ((w
                   (prog1 (car words)
                     (setq words (cdr words)))))
              (when (string-match prefix-as-regex w nil)
                ;; Check the string isn't the end of the symbol (that there is text to add).
                (unless (equal (match-end 0) (length w))
                  (let ((w-test (substring w (match-end 0) (length w))))
                    (when trailing-text
                      (setq w-test
                            (mono-complete-backend-whole-word--trim-suffix-or-nil
                             trailing-text w-test)))
                    (when w-test
                      (setq result (list w-test)))))))))))

    ;; No match, empty result.
    (unless result
      (setq result (list "")))

    (cons result cache)))

(defun mono-complete-backend-whole-word-complete (_config prefix cache)
  "Complete at point based on PREFIX & CACHE."
  (let ((trailing-text
         (cond
          ((eq mono-complete-backend-whole-word-trailing-text 'match)
           (mono-complete-backend-whole-word--tailing-text-as-string (point)))
          (t
           nil))))

    (cond
     ;; When in a mini-buffer, use the original buffer as the source.
     ((minibufferp)
      (let ((final-result nil))
        (let ((parent-buf (window-buffer (minibuffer-selected-window))))
          (when parent-buf
            (with-current-buffer parent-buf
              (setq final-result
                    (mono-complete-backend-whole-word--complete-impl
                     prefix cache trailing-text)))))
        (or final-result (cons nil cache))))
     (t
      (mono-complete-backend-whole-word--complete-impl prefix cache trailing-text)))))

;; ---------------------------------------------------------------------------
;; Public Callback

;;;###autoload
(defun mono-complete-backend-whole-word ()
  "Whole word completer."
  (list
   :prefix #'mono-complete-backend-whole-word-prefix
   :complete #'mono-complete-backend-whole-word-complete))

(provide 'mono-complete-backend-whole-word)
;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; elisp-autofmt-format-quoted: nil
;; End:
;;; mono-complete-backend-whole-word.el ends here
