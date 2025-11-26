;;; mono-complete-backend-word-predict.el --- DABBREV back-end -*- lexical-binding: t -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2023  Campbell Barton
;; URL: https://codeberg.org/ideasman42/emacs-mono-complete
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;; Word predict back-end.

;;; Code:

(require 'mono-complete)


;; ---------------------------------------------------------------------------
;; Custom Variables

(defgroup mono-complete-backend-word-predict nil
  "Word prediction for mono-complete, generated on demand."
  :group 'convenience)

(defcustom mono-complete-backend-word-predict-input-paths-match-source
  (list
   "*.c" ; C.
   "*.cc" ; C++.
   "*.cpp" ; C++.
   "*.cxx" ; C++.
   "*.el" ; EMacs-lisp.
   "*.glsl" ; OpenGL shading language.
   "*.go" ; GO language.
   "*.h" ; C header.
   "*.hh" ; C++ header.
   "*.hxx" ; C++ header.
   "*.java" ; Java.
   "*.js" ; Java-Script.
   "*.lua" ; Lua.
   "*.m" ; Objective-C.
   "*.mm" ; Objective-C++.
   "*.py" ; Python.
   "*.rb" ; Ruby.
   "*.rs" ; Rust.
   )
  "Source files to include when scanning directories for files to extract.
Files are parsed as code and comments are extracted.
Each entry is a UNIX style glob."
  :type (list 'repeat 'string))

(defcustom mono-complete-backend-word-predict-input-paths-match-text (list "*.rst" "*.md" "*.txt")
  "Text files to include when scanning directories for files to extract.
Files are parsed as plain-text, all words are extracted.
Each entry is a UNIX style glob."
  :type (list 'repeat 'string))

(defcustom mono-complete-backend-word-predict-input-paths-size-limit 524288
  "Files above this size will be ignored when scanning directories recursively.
This can avoid slow parsing for source files which are used as data-storage.
Zero disabled size limit checks."
  :type 'integer)

(defcustom mono-complete-backend-word-predict-input-paths (list "")
  "Paths used for extracting text.
- A blank string (default) will be replaced by the current buffers project root.
- A file will be read.
- A directory will be scanned recursively for files matching
  `mono-complete-backend-word-predict-input-paths-match-source' or
  `mono-complete-backend-word-predict-input-paths-match-text'."
  :type (list 'repeat 'string))

(defcustom mono-complete-backend-word-predict-update-method 'when-missing
  "Method used for validating the model.
- \"From Manifest\" rebuilds cache that has become outdated.
- \"When Missing\" only generates data when the cache is not found.
  This can be used when users prefer to manually update cache."
  :type
  '(choice (const :tag "From Manifest" from-manifest)
           (const :tag "When Missing" when-missing)))

(defcustom mono-complete-backend-word-predict-ngram-max 5
  "The n-gram size.
2 or more, avoid values over 4 (or accept very large data-bases)."
  :type 'integer)

(defconst mono-complete-backend-word-predict--command
  (concat
   (file-name-directory (or load-file-name buffer-file-name))
   "mono-complete-backend-word-predict.py"))

;; (defconst
;;   mono-complete-backend-word-predict--command
;;   "/src/emacs/mono-complete/mono-complete-backend-word-predict.py")

;; ---------------------------------------------------------------------------
;; Internal Utilities

(defun mono-complete-backend-word-predict--range-contains (beg end re-contains)
  "Return non-nil if BEG END range containing RE-CONTAINS."
  (save-excursion
    (goto-char beg)
    (eq (- end beg) (skip-chars-forward re-contains end))))

(defun mono-complete-backend-word-predict--prefix-and-words (is-partial)
  "Return the prefix and word list.
When IS-PARTIAL is non-nil, an extra word is required."
  (let ((words-limit-max
         (+ mono-complete-backend-word-predict-ngram-max
            (cond
             ;; Allow an extra word for partial completion.
             (is-partial
              1)
             (t
              0))))
        (words-limit-min
         (cond
          ;; Allow an extra word for partial completion.
          (is-partial
           3)
          (t
           2)))
        (pos-init (point))
        (pos-step-prev nil)
        (pos-beg nil)
        (words (list))
        (search t))
    (save-excursion
      (while (and search (< (length words) words-limit-max) (null (eq pos-step-prev (point))))
        (setq pos-step-prev (point))
        (skip-chars-backward "\n[:blank:][:punct:]")

        ;; Early exit on full-stop, ! ... etc.
        (cond
         ((null
           (mono-complete-backend-word-predict--range-contains
            (point) pos-step-prev "^.!?:;)\\]}"))
          (setq search nil))
         (t
          (let ((pos-word-end (point)))
            (skip-chars-backward "^\n[:blank:]")
            (let ((pos-word-beg (point)))
              (let ((word (buffer-substring-no-properties pos-word-beg pos-word-end)))
                ;; Ensure the word is ONLY A-Z-A-z and apostrophe.
                (cond
                 ((and (< pos-word-beg pos-word-end)
                       (string-empty-p (string-trim-left word "[[:alnum:]'-]*")))
                  (push word words)
                  (setq pos-beg pos-word-beg))
                 (t
                  (setq search nil))))))))))

    (mono-complete--debug-log "complete words: %S" words)
    (cond
     ((<= words-limit-min (length words))
      (cons (buffer-substring-no-properties pos-beg pos-init) words))
     (t
      (cons nil nil)))))

(defun mono-complete-backend-word-predict--command-to-string (command &rest args)
  "Execute shell COMMAND with ARGS and return its output as a string."
  ;; Handy to run outside of emacs for debugging the script it's self.
  ;; (printf "command: %S\n" args)
  (with-temp-buffer
    (let ((all-args
           (nconc
            (list
             ;; Command.
             command
             ;; In-file.
             nil
             ;; Destination (stdout).
             (current-buffer)
             ;; Display.
             nil)
            args))

          ;; Ensure that `default-directory' exists and is readable.
          ;; Even though the default directory isn't used, `call-process'
          ;; will fail with an error when called with a buffer open that
          ;; references a directory that doesn't exist.
          ;; Assume the users home directory is valid and use this instead.
          (default-directory (expand-file-name "~")))

      (apply #'call-process all-args)
      (buffer-string))))

(defun mono-complete-backend-word-predict--input-paths-scan ()
  "Return root directories."
  (delq
   nil
   (mapcar
    (lambda (path)
      (let ((path-expanded
             (cond
              ((string-empty-p path)
               (mono-complete-project-root))
              (t
               (expand-file-name path)))))
        (when path-expanded
          (unless (file-exists-p path-expanded)
            (message
             "mono-complete-backend-word-predict-root-directories: skipping missing directory %S"
             path-expanded)
            (setq path-expanded nil)))
        path-expanded))
    mono-complete-backend-word-predict-input-paths)))

(defun mono-complete-backend-word-predict--run-ext-util (text partial-word update-method)
  "Run the external word prediction utility on TEXT.
PARTIAL-WORD may be an empty string,otherwise part of the word to complete.
UPDATE-METHOD the method used to check if the method needs to be updated."
  (mono-complete-backend-word-predict--command-to-string
   "python"
   mono-complete-backend-word-predict--command

   ;; Cache directory.
   "--cache"
   (file-name-concat (expand-file-name mono-complete-cache-directory) "word-predict")

   ;; Text to complete (or nothing to generate).
   "--text"
   text

   ;; Complete using the word behind the cursor as a partial
   "--partial-text"
   partial-word

   ;; Multiple roots.
   "--input-paths"
   (mapconcat #'identity (mono-complete-backend-word-predict--input-paths-scan) path-separator)

   ;; Input files size limit.
   "--input-paths-size-limit"
   (number-to-string mono-complete-backend-word-predict-input-paths-size-limit)

   ;; Include source extensions.
   "--input-paths-match-source"
   (mapconcat #'identity mono-complete-backend-word-predict-input-paths-match-source
              path-separator)
   ;; Include text extensions.
   "--input-paths-match-text"
   (mapconcat #'identity mono-complete-backend-word-predict-input-paths-match-text path-separator)

   ;; Update method.
   "--update"
   update-method))


;; ---------------------------------------------------------------------------
;; Callbacks

(defun mono-complete-backend-word-predict-setup (config)
  "Setup on enabled mode (for this buffer).
TODO: support CONFIG."
  ;; Blank "text" is a signal to generate all models.
  (let ((text
         (mono-complete-backend-word-predict--run-ext-util
          "" "" (symbol-name mono-complete-backend-word-predict-update-method))))
    (unless (string-empty-p text)
      (message "%s" text)))
  config)

(defun mono-complete-backend-word-predict-prefix ()
  "Return the prefix at point."
  (let ((is-partial nil))
    (when (cond
           ;; After space, search for whole word.
           ((eq ?\s (preceding-char))
            (mono-complete--debug-log "look for new word")
            t)
           ;; Typing in word, check if this word is a part of a longer word.
           ((mono-complete-backend-word-predict--range-contains (1- (point)) (point) "[:alpha:]'-")
            (mono-complete--debug-log "look for partial word, or new word (fallback)")
            (setq is-partial t)
            t)
           (t
            ;; When directly after non-ascii.
            (mono-complete--debug-log
             "look for word exiting, word chars or white-space not found before character")
            nil))

      (pcase-let ((`(,prefix . ,words)
                   (mono-complete-backend-word-predict--prefix-and-words is-partial)))

        (when prefix
          (when (add-text-properties 0 1 (list 'mono-complete-backend-word-predict words) prefix)
            prefix))))))

(defun mono-complete-backend-word-predict-complete (_config prefix cache)
  "Complete at point based on PREFIX & CACHE."
  ;; Return a list of strings or nil.
  (let ((result nil)
        (partial-word "")
        (words (get-text-property 0 'mono-complete-backend-word-predict prefix)))

    (unless (eq ?\s (preceding-char))
      ;; Pop last, TODO: maybe worth a utility.
      (setq words (nreverse words))
      (setq partial-word (pop words))
      (setq words (nreverse words)))

    ;; At least two words is always needed.
    (when (>= (length words) 2)
      (let ((text
             (mono-complete-backend-word-predict--run-ext-util
              (mapconcat #'identity words " ") partial-word
              ;; Updating should be handled when the mode is enabled.
              "when-missing")))
        (unless (string-empty-p text)
          (setq result (list text)))))

    (cons result cache)))


;; ---------------------------------------------------------------------------
;; Public Callback

;;;###autoload
(defun mono-complete-backend-word-predict ()
  "DEBBREV completer."
  (list
   :setup #'mono-complete-backend-word-predict-setup
   :prefix #'mono-complete-backend-word-predict-prefix
   :complete #'mono-complete-backend-word-predict-complete))

(provide 'mono-complete-backend-word-predict)
;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; elisp-autofmt-format-quoted: nil
;; End:
;;; mono-complete-backend-word-predict.el ends here
