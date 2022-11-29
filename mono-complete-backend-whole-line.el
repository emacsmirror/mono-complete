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

(defun mono-complete-backend-whole-line-prefix ()
  "Return the prefix at point."
  (save-excursion
    (let ((pos-init (point)))
      (back-to-indentation)
      (cond
       ((< (point) pos-init)
        (buffer-substring-no-properties pos-init (point)))
       (t
        nil)))))

(defun mono-complete-backend-whole-line-complete (_config prefix cache)
  "Complete at point based on PREFIX & CACHE."
  (let ((result nil) ; Return a list of strings or nil.
        (search-re (concat "^[[:blank:]]*" (regexp-quote prefix)))
        (pos-init (point)))

    (save-match-data
      (save-excursion

        ;; Search backwards.
        (beginning-of-line)
        (while (and (null result) (re-search-backward search-re nil t))
          (let ((pos-beg (match-end 0)))
            (end-of-line)
            (skip-chars-backward "[:blank:]" pos-beg)
            (when (< pos-beg (point))
              (setq result (list (buffer-substring-no-properties pos-beg (point)))))))

        (unless result
          (goto-char pos-init)

          ;; Search forwards.
          (end-of-line)
          (while (and (null result) (re-search-forward search-re nil t))
            (let ((pos-beg (match-end 0)))
              (end-of-line)
              (skip-chars-backward "[:blank:]" pos-beg)
              (when (< pos-beg (point))
                (setq result (list (buffer-substring-no-properties pos-beg (point))))))))))
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
;; End:
;;; mono-complete-backend-whole-line.el ends here
