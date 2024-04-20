;;; mono-complete-backend-filesystem.el --- DABBREV back-end -*- lexical-binding: t -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2023  Campbell Barton
;; URL: https://codeberg.org/ideasman42/emacs-mono-complete
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;; Whole line back-end.
;
;;; Code:


;; ---------------------------------------------------------------------------
;; Internal Utilities

;; Native path separator.
(defconst mono-complete-backend-filesystem--sep-char
  (cond
   ((memq system-type '(windows-nt ms-dos))
    ?\\)
   (t
    ?/)))

(defun mono-complete-backend-filesystem--split-path (filepath)
  "Split FILEPATH \"/a/b/c\" into (\"/a/b/\" . \"c\")."
  (let ((filename (file-name-nondirectory filepath))
        (filepath-len (length filepath)))
    (cond
     ((length= filename filepath-len)
      (cons filepath nil))
     (t
      (cons (substring filepath 0 (- filepath-len (length filename))) filename)))))

(defun mono-complete-backend-filesystem--expand-path (filepath)
  "Expand FILEPATH based on local or home directory (as needed)."
  (cond
   ((string-empty-p filepath)
    filepath)
   (t
    (let ((ch (aref filepath 0)))
      (cond
       ((eq ch ?.)
        (concat
         (or (and buffer-file-name (file-name-directory buffer-file-name))
             default-directory)
         (substring filepath 1 nil)))
       ((eq ch ?~)
        (concat (expand-file-name "~") (substring filepath 1 nil)))
       (t
        filepath))))))


;; ---------------------------------------------------------------------------
;; Callback Implementations

(defun mono-complete-backend-filesystem-prefix ()
  "Return the prefix at point."
  (let ((prefix nil)
        (sep-chr mono-complete-backend-filesystem--sep-char))

    ;; Skip when the previous character is a:
    ;; - Slash: this is a complete path, don't attempt completion.
    ;; - White-space: while technically these could be used,
    ;;   will attempt completion in many cases where it doesn't make much sense,
    ;;   ignore as the user can simply type in the non-space character to trigger completion.
    (unless (memq (preceding-char) (list sep-chr 0 ?\s ?\t ?\n))
      (save-excursion
        (let* ((pos-init (point))
               (pos-bol
                (progn
                  (beginning-of-line)
                  (skip-chars-forward "[:blank:]" pos-init)
                  (point)))
               (pos-beg nil)
               (search t))
          (unless (eq pos-init pos-bol)
            (let ((sep-skip (concat "^" (char-to-string sep-chr))))
              (while search
                (let ((ch (following-char)))
                  (when (eq ch sep-chr)
                    (setq pos-beg (point))
                    (let ((ch-prev (preceding-char)))
                      (cond
                       ((eq ch-prev ?.)
                        (setq pos-beg (1- pos-beg)))
                       ((eq ch-prev ?~)
                        (setq pos-beg (1- pos-beg)))))
                    (let ((prefix-test (buffer-substring-no-properties pos-beg pos-init)))
                      (pcase-let ((`(,directory . ,filename)
                                   (mono-complete-backend-filesystem--split-path prefix-test)))

                        (when (file-directory-p
                               (mono-complete-backend-filesystem--expand-path directory))

                          ;; Break out of the loop, even if `prefix' is not set.
                          ;; Since the directory exists, searching further makes no sense.
                          (setq search nil)
                          (cond
                           (filename
                            (setq prefix prefix-test))
                           (t
                            ;; This is a complete path,
                            ;; stop searching but don't prevent further completion.
                            (setq prefix ""))))))))
                (when (zerop (skip-chars-forward sep-skip pos-init))
                  (setq search nil))))))

        (unless (and prefix (stringp prefix))
          (setq prefix nil))))

    prefix))

(defun mono-complete-backend-filesystem-complete (_config prefix cache)
  "Complete at point based on PREFIX & CACHE."
  ;; Note that cache is the:
  ;; (path . sorted-files)

  ;; Return a list of strings or nil.
  (let ((result nil))
    (pcase-let ((`(,directory . ,filename) (mono-complete-backend-filesystem--split-path prefix)))
      ;; Initialize cache.
      (cond
       (filename
        (unless cache
          ;; Always overwrite next.
          (setq cache (cons "" nil)))
        (unless (string-equal directory (car cache))
          (setcar cache directory)
          (setcdr
           cache
           (sort (file-name-all-completions
                  ""
                  (mono-complete-backend-filesystem--expand-path directory))
                 #'string-lessp)))

        (let ((files (cdr cache)))
          (while files
            (let ((filename-complete (pop files)))
              (cond
               ((string-prefix-p filename filename-complete)
                (setq result (list (substring filename-complete (length filename) nil)))
                ;; On an exact match, keep searching as there may be longer names.
                (unless (string-empty-p (car result))
                  ;; Break.
                  (setq files nil)))
               (t ; As this is a sorted list, early exit when the prefix no longer matches.
                (when result
                  ;; Break.
                  (setq files nil))))))))
       (t
        ;; No filename, empty completion.
        (setq result (list "")))))

    (cons result cache)))


;; ---------------------------------------------------------------------------
;; Public Callback

;;;###autoload
(defun mono-complete-backend-filesystem ()
  "DEBBREV completer."
  (list
   :prefix #'mono-complete-backend-filesystem-prefix
   :complete #'mono-complete-backend-filesystem-complete))

(provide 'mono-complete-backend-filesystem)
;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; End:
;;; mono-complete-backend-filesystem.el ends here
