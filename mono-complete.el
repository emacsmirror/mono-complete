;;; mono-complete.el --- Completion suggestions with multiple back-ends -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2023  Campbell Barton

;; Author: Campbell Barton <ideasman42@gmail.com>

;; URL: https://codeberg.org/ideasman42/emacs-mono-complete
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Configurable completion suggestions while typing.

;;; Usage

;; (mono-complete-mode)

;;; Code:


;; ---------------------------------------------------------------------------
;; Custom Variables

(defgroup mono-complete nil
  "Complete while typing with configurable back-ends."
  :group 'convenience)

(defcustom mono-complete-backends (list 'dabbrev)
  "A list of backend identifiers, or a function which returns the same.

When a function is used this takes a single boolean IS-CONTEXT argument.
When non-nil return all back-ends that may be used for the buffer,
otherwise return a sub-set of this list based on the current context."
  :type '(repeat function))

(defcustom mono-complete-preview-delay 0.235
  "How long to wait until displaying the preview after a keystroke (in seconds)."
  :type 'float)

(defcustom mono-complete-self-insert-commands '(self-insert-command org-self-insert-command)
  "A list of commands after which to show a preview."
  :type '(repeat function))

(defcustom mono-complete-fallback-command 'indent-for-tab-command
  "Command to run when no preview is available."
  :type 'function)

(defcustom mono-complete-literal-input t
  "Simulate literal text input.

When enabled replaying this action as a macro re-inserts the literal text
instead of performing the completion action (which may give different results)."
  :type 'boolean)

(defcustom mono-complete-evil-insert-mode-only t
  "Restrict to insert mode when used in combination with `evil-mode'."
  :type 'boolean)

(defcustom mono-complete-meow-insert-mode-only t
  "Restrict to insert mode when used in combination with `meow-mode'."
  :type 'boolean)

(defvar mono-complete-generic-insert-mode-functions nil
  "Restrict to insert mode when used in combination with modal editing.
When non-nil this must be a list of 3 symbols referencing functions.
- Predicate function (return non-null when the mode is enabled).
- Enter hook.
- Exit hook.")

(defcustom mono-complete-cache-directory
  (locate-user-emacs-file "mono-complete" ".emacs-mono-complete")
  "The directory to store mono-complete cache data."
  :type 'string)

(defface mono-complete-preview-face '((t (:foreground "#ffff00" :background "#000000")))
  "Face for the preview.")


;; ---------------------------------------------------------------------------
;; Custom Variables (used by back-ends)

(defcustom mono-complete-project-root 'mono-complete-project-root-default
  "Function to call that returns the root path of the current buffer.
A nil return value will fall back to the `default-directory'."
  :type 'function)


;; ---------------------------------------------------------------------------
;; Custom Callbacks

(defcustom mono-complete-debug-log nil ; 'stdout
  "Debug logging (intended for back-end developers)."
  :type
  '(choice (const :tag "Disabled" nil)
           (const :tag "Buffer" t)
           (const :tag "Standard Output" stdout)))

(defcustom mono-complete-debug-log-time t
  "Report the time taken to execute back-ends completion functions.

Intended for back-end developers investigating performance."
  :type 'boolean)

(defcustom mono-complete-debug-log-backends t
  "Report the back-ends used."
  :type 'boolean)


;; ---------------------------------------------------------------------------
;; Public Variables

(defvar mono-complete-mode-map (make-sparse-keymap)
  "Minimal key-map intended to call.
`mono-complete-expand' or `mono-complete-expand-or-fallback'.")


;; ---------------------------------------------------------------------------
;; Compatibility

(when (version< emacs-version "31.1")
  (defmacro incf (place &optional delta)
    "Increment PLACE by DELTA or 1."
    (declare (debug (gv-place &optional form)))
    (gv-letplace (getter setter) place
      (funcall setter `(+ ,getter ,(or delta 1)))))
  (defmacro decf (place &optional delta)
    "Decrement PLACE by DELTA or 1."
    (declare (debug (gv-place &optional form)))
    (gv-letplace (getter setter) place
      (funcall setter `(- ,getter ,(or delta 1))))))


;; ---------------------------------------------------------------------------
;; Generic Functions

(defun mono-complete-project-root-default ()
  "Function to find the project root from the current buffer.
This checks `ffip', `projectile' & `vc' root,
using `default-directory' as a fallback."
  (declare (important-return-value t))
  (cond
   ((fboundp 'ffip-project-root)
    (funcall #'ffip-project-root))
   ((fboundp 'projectile-project-root)
    (funcall #'projectile-project-root))
   (t
    (or (when buffer-file-name
          (let ((vc-backend
                 (ignore-errors
                   (vc-responsible-backend buffer-file-name))))
            (when vc-backend
              (vc-call-backend vc-backend 'root buffer-file-name))))))))

(defun mono-complete-project-root ()
  "Return the project directory (or default)."
  (declare (important-return-value t))
  ;; Needed in case the path is relative or begins with "~" for HOME.
  ;; If the input to `file-name-as-directory' is an empty string,
  ;; this gets converted to "./" the expanded to the absolute location.
  (expand-file-name
   (file-name-as-directory (or (funcall mono-complete-project-root) default-directory))))


;; ---------------------------------------------------------------------------
;; Internal Variables

;; Cache for back-end presets, avoid requiring them and calling their function.
(defvar mono-complete--backend-require-cache nil)

;; The preview overlay or nil.
(defvar-local mono-complete--preview-overlay nil)

;; The preview overlay state or nil when the command.
;; This is the result of `mono-complete--preview-state-from-overlay' see it's doc-string for details.
(defvar-local mono-complete--preview-overlay-was-visible nil)

;; The preview idle timer.
(defvar-local mono-complete--preview-timer nil)

;; Store the current context during text insertion.
;; - `backends':
;;   Store a list of back-ends calculated when typing begins.
;;
;; - `result-cache':
;;   Hash where:
;;   - The key is `complete-fn'.
;;   - The value is a cons cell where:
;;     - The CAR is the prefix,
;;     - The CDR is the cache value defined by the completion implementation
;;       (passed to and return from `complete-fn').

(defvar-local mono-complete--context nil)


;; ---------------------------------------------------------------------------
;; Internal Constants

(defconst mono-complete--commands '(mono-complete-expand mono-complete-expand-or-fallback))

;; Boolean to use this to prevent simulated input running command hooks
;; (which would otherwise trigger the idle timer). Use `let' to override this.
(defconst mono-complete--suppress-command-hooks nil)


;; ---------------------------------------------------------------------------
;; Internal Logging

(defsubst mono-complete--debug-log-unchecked (&rest args)
  "Log format ARGS."
  (let ((str (apply #'format args)))
    (cond
     ((eq 'stdout mono-complete-debug-log)
      (princ str #'external-debugging-output)
      (external-debugging-output ?\n))
     (t
      (let ((buf (get-buffer-create "*mono-complete-log*")))
        (with-current-buffer buf
          (insert str "\n")))))))

(defsubst mono-complete--debug-log (&rest args)
  "Log format ARGS."
  (when mono-complete-debug-log
    (apply #'mono-complete--debug-log-unchecked args)))


;; ---------------------------------------------------------------------------
;; Internal Macro Utilities

(defun mono-complete--interactive-or-non-literal-input ()
  "Return non-nil if this command is interactive or literal input is disabled."
  (declare (important-return-value t))
  (cond
   (mono-complete-literal-input
    ;; Interactive only, when non-interactive,
    ;; the macros called here will be in-lined
    ;; and there is no need to perform any functionality in that case.
    (not (or executing-kbd-macro noninteractive)))
   (t
    t)))

(defun mono-complete--key-from-command (fn &optional descriptionp)
  "Return the key for command symbol FN.
When DESCRIPTIONP is non-nil, return it's description."
  (declare (important-return-value t))
  (unless (commandp fn)
    (error "Not a command: %s" fn))
  (let ((key
         (car (where-is-internal (or (command-remapping fn) fn) overriding-local-map nil nil))))
    (cond
     ((null key)
      nil)
     (descriptionp
      (key-description key))
     (t
      key))))

(defun mono-complete--call-interactively-macro (command-symbol)
  "Call COMMAND-SYMBOL as a macro."
  (declare (important-return-value t))
  (let ((command (symbol-name command-symbol))
        (binding (mono-complete--key-from-command command-symbol t)))
    (unless binding
      ;; Attempt to run "M-x command" if there is no direct shortcut.
      (setq binding
            (concat
             (or (mono-complete--key-from-command 'execute-extended-command t) "M-x")
             " "
             command)))
    (execute-kbd-macro (read-kbd-macro binding))))

(defun mono-complete--insert-with-literal-input (text)
  "Helper function to simulate input using TEXT."
  (declare (important-return-value nil))
  (with-undo-amalgamate
    (execute-kbd-macro (vconcat text))))

(defun mono-complete--backend-load-validate-uuid (id uuid config)
  "Validate ID, UUID & CONFIG arguments."
  (declare (important-return-value t))
  (unless uuid
    (cond
     (config
      (message
       "mono-complete-backend-load: has CONFIG argument without a UUID argument, skipping!")
      (setq uuid :invalid))
     (t
      (setq uuid id))))
  uuid)


;; ---------------------------------------------------------------------------
;; Internal Back-end Functions

(defun mono-complete--backend-load-impl (id &optional uuid config)
  "See `mono-complete-backend-load' for ID UUID & CONFIG doc-strings."
  (declare (important-return-value t))
  (unless mono-complete--backend-require-cache
    (setq mono-complete--backend-require-cache (make-hash-table :test #'eq)))

  (let ((result (gethash uuid mono-complete--backend-require-cache :unset)))
    (when (eq result :unset)
      (setq result nil)

      (let ((preset-sym (intern (concat "mono-complete-backend-" (symbol-name id)))))
        (when (condition-case-unless-debug err
                  (progn
                    (require preset-sym)
                    t)
                (error
                 (message "mono-complete: back-end %S not found! (%S)" preset-sym err)
                 nil))
          (setq result (funcall preset-sym))))

      ;; Put the result in the hash even when it's nil, not to regenerate.
      (puthash id result mono-complete--backend-require-cache))

    (when (and result config)
      (plist-put result :config config))

    result))

(defun mono-complete-backend-load (id &optional uuid config)
  "Load a pre-defined back-end ID.
When passing in a CONFIG UUID must be a unique identifier in the list."
  (declare (important-return-value t))
  (cond
   ((and id (symbolp id))
    (setq uuid (mono-complete--backend-load-validate-uuid id uuid config))
    (cond
     (uuid
      (mono-complete--backend-load-impl id uuid config))
     (t
      nil)))
   (t
    (message "mono-complete: found non-symbol when loading a back-end (%S)" id)
    nil)))

(defun mono-complete--backends-from-config (is-context)
  "Return back-ends from user configuration.
IS-CONTEXT is forwarded to the callback."
  (declare (important-return-value t))
  (let ((backends mono-complete-backends))

    (when (functionp backends)
      (setq backends (funcall backends is-context)))

    (mapcar (lambda (id) (mono-complete-backend-load id)) backends)))


;; ---------------------------------------------------------------------------
;; Internal Functions

(defun mono-complete--is-mono-complete-command (command)
  "Return non-nil if COMMAND is a mono-complete command."
  (declare (important-return-value t))
  (memq command mono-complete--commands))

(defun mono-complete--is-self-insert-command (command)
  "Return non-nil if COMMAND is a \"self-insert command\"."
  (declare (important-return-value t))
  (memq command mono-complete-self-insert-commands))

(defun mono-complete--preview-text-at-point ()
  "Show the completion from the text at the point (where possible)."
  (declare (important-return-value t))
  (let ((result nil)
        (backends-cons (assq 'backends mono-complete--context))
        (backends nil)
        (prefix-cache (list)))

    (cond
     (backends-cons
      (setq backends (cdr backends-cons)))
     (t
      (setq backends (mono-complete--backends-from-config t))
      (setq backends-cons (cons 'backends backends))
      (push backends-cons mono-complete--context)))

    (when (and mono-complete-debug-log mono-complete-debug-log-backends)
      (let ((backend-info (list)))
        (dolist (backend-item backends)
          (when backend-item
            (pcase-let ((`(,_config ,_setup-fn ,_prefix-fn ,complete-fn)
                         (mono-complete--backend-items-or-warn backend-item)))
              (when complete-fn
                (let ((backend-str
                       ;; Remove prefix for brevity only.
                       (string-remove-prefix "mono-complete-backend-" (format "%S" complete-fn))))
                  (push backend-str backend-info))))))
        (setq backend-info (nreverse backend-info))
        (mono-complete--debug-log-unchecked
         "backend-used: (%d) %s" (length backend-info) (mapconcat #'identity backend-info ", "))))

    (while backends
      (let ((backend-item (pop backends)))
        (when backend-item
          (pcase-let ((`(,config ,_setup-fn ,prefix-fn ,complete-fn)
                       (mono-complete--backend-items-or-warn backend-item)))
            (when complete-fn
              (let ((prefix nil))
                (let ((prefix-fn-result-cons (assq prefix-fn prefix-cache)))
                  (cond
                   (prefix-fn-result-cons
                    (setq prefix (cdr prefix-fn-result-cons)))
                   (t
                    (condition-case-unless-debug err
                        (setq prefix (funcall prefix-fn))
                      (error
                       (message "mono-complete: prefix function %S, failed with error (%S)"
                                prefix-fn
                                err)))
                    (push (cons prefix-fn prefix) prefix-cache))))

                ;; There may be no prefix, in this case skip.
                (when prefix
                  (let ((backend-cache (mono-complete--backend-cache-ensure complete-fn)))
                    (cond
                     ;; When the prefix was previously ignored, do nothing.
                     ((and (stringp (car backend-cache))
                           (string-prefix-p (car backend-cache) prefix)))

                     ;; Call the completion function.
                     ((let ((result-suffix
                             (mono-complete--backend-call-and-update
                              complete-fn config prefix backend-cache)))
                        (when result-suffix
                          (setq result (cons prefix result-suffix))))

                      ;; Break.
                      (setq backends nil))
                     (t
                      ;; Skip this prefix in the future to prevent excessive calculation.
                      (setcar backend-cache prefix)))))))))))
    result))

(defun mono-complete--on-exit ()
  "Function run when executing another command.

That is, if `this-command' is not one of `mono-complete--commands'."
  (declare (important-return-value nil))
  (mono-complete--backend-cache-clear)
  (setq mono-complete--context nil))


;; ---------------------------------------------------------------------------
;; Internal Back-End Functions

(defun mono-complete--backend-call-and-update (complete-fn config prefix backend-cache)
  "Call COMPLETE-FN with CONFIG, PREFIX & update BACKEND-CACHE."
  (declare (important-return-value t))
  (let ((time-beg nil))
    (when (and mono-complete-debug-log mono-complete-debug-log-time)
      (setq time-beg (current-time)))

    (pcase-let ((`(,result . ,cache-next) (funcall complete-fn config prefix (cdr backend-cache))))
      (when time-beg
        (mono-complete--debug-log-unchecked
         "backend-call: (%S) %.4f"
         complete-fn
         (float-time (time-subtract (current-time) time-beg))))

      (setcdr backend-cache cache-next)
      result)))

(defun mono-complete--backend-cache-set (complete-fn val)
  "Set VAL for COMPLETE-FN."
  (declare (important-return-value nil))
  (let ((result-cache-cons (assq 'result-cache mono-complete--context))
        (result-cache nil))
    (cond
     (result-cache-cons
      (setq result-cache (cdr result-cache-cons)))
     (t
      (setq result-cache (make-hash-table :test #'eq))
      (setq result-cache-cons (cons 'result-cache result-cache))
      (push result-cache-cons mono-complete--context)))

    (puthash complete-fn val result-cache)))

(defun mono-complete--backend-cache-ensure (complete-fn)
  "Ensure COMPLETE-FN has an entry in `mono-complete--context' (result-cache)."
  (declare (important-return-value t))
  (let ((result-cache (alist-get 'result-cache mono-complete--context nil nil #'eq)))
    (or
     ;; Existing.
     (and result-cache (gethash complete-fn result-cache))
     ;; Add new.
     (mono-complete--backend-cache-set complete-fn (cons nil nil)))))

(defun mono-complete--backend-cache-clear ()
  "Clear back-end cache."
  (declare (important-return-value nil))
  ;; Get and remove, the key.
  (let ((result-cache (alist-get 'result-cache mono-complete--context nil t #'eq)))
    (when result-cache
      (clrhash result-cache))))

(defun mono-complete--backend-items-or-warn (item)
  "Extract back-end callbacks from ITEM, returning a list or nil."
  (declare (important-return-value t))
  (let ((config nil)
        ;; Setup is optional.
        (setup-fn nil)
        (prefix-fn nil)
        (complete-fn nil))
    (while item
      (let* ((key (pop item))
             (val (pop item)))
        (cond
         ((eq key :config)
          (setq config val))
         ((eq key :setup)
          (setq setup-fn val))
         ((eq key :prefix)
          (setq prefix-fn val))
         ((eq key :complete)
          (setq complete-fn val))
         (t
          (message "Unexpected keyword %S found!" key)))))

    (cond
     ((eq config t)
      ;; A signal that calling setup failed (with an error or the mode was not compatible),
      ;; return nothing with no error.
      nil)
     ((null complete-fn)
      (message "Missing :complete function!")
      nil)
     ((null prefix-fn)
      (message "Missing :prefix function!")
      nil)
     (t
      (list config setup-fn prefix-fn complete-fn)))))


;; ---------------------------------------------------------------------------
;; Internal Preview Functions

(defun mono-complete--preview-state-from-overlay ()
  "Return the state of the overlay: (position . (prefix . expansion))."
  (declare (important-return-value t))
  (when (and mono-complete--preview-overlay (overlay-buffer mono-complete--preview-overlay))
    (cons
     (overlay-start mono-complete--preview-overlay)
     (cons
      (overlay-get mono-complete--preview-overlay 'mono-complete-prefix)
      (overlay-get mono-complete--preview-overlay 'after-string)))))

(defun mono-complete--preview-create-overlay (prefix expansion)
  "Add EXPANSION overlay (with PREFIX as a property)."
  (declare (important-return-value t))
  (let ((overlay (make-overlay (point) (point))))
    ;; Empty strings may be used for temporary expansion.
    (unless (string-empty-p expansion)
      (add-text-properties 0 1 '(cursor 1) expansion))
    (add-face-text-property 0 (length expansion) 'mono-complete-preview-face nil expansion)

    (overlay-put overlay 'after-string expansion)
    (overlay-put overlay 'mono-complete-prefix prefix)

    overlay))

(defun mono-complete--preview-refresh-from-state (state)
  "Detect when text insertion follows the current preview allowing it to be used.
Argument STATE is the result of `mono-complete--preview-state-from-overlay'."
  (declare (important-return-value t))
  (let ((result nil))
    (when state
      (pcase-let ((`(,pos-prev . (,prefix-prev . ,expansion-prev)) state))
        ;; Ensure the point didn't move backwards.
        (when (<= pos-prev (point))
          ;; When the length is equal, the entire word was manually typed in.
          (when (> (length expansion-prev) (- (point) pos-prev))
            (let ((prefix-in-buffer
                   (buffer-substring-no-properties (- pos-prev (length prefix-prev)) pos-prev)))
              ;; Sanity check that the buffer prefix has not changed.
              (when (string-equal prefix-prev prefix-in-buffer)
                (let ((overlap (buffer-substring-no-properties pos-prev (point))))
                  (when (or (string-empty-p overlap) (string-prefix-p overlap expansion-prev))
                    ;; The modifications made don't impact the
                    (let ((prefix (concat prefix-prev overlap))
                          (expansion (substring-no-properties expansion-prev (length overlap))))

                      (when mono-complete--preview-overlay
                        ;; Should never happen, just sanity check.
                        (error "Invalid internal state"))

                      (setq mono-complete--preview-overlay
                            (mono-complete--preview-create-overlay prefix expansion))

                      (setq result t)))))))))

      ;; Don't refresh, use the timer instead.
      result)))

(defun mono-complete--preview-text-from-command ()
  "Return the expansion text for the preview displayed when the command began."
  (declare (important-return-value t))
  (when mono-complete--preview-overlay-was-visible
    (substring-no-properties (cdr (cdr mono-complete--preview-overlay-was-visible)))))

(defun mono-complete--preview (buf)
  "Show the preview for BUF."
  (declare (important-return-value nil))
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (cancel-timer mono-complete--preview-timer)
      (setq mono-complete--preview-timer nil)

      (let ((expansion-pair (mono-complete--preview-text-at-point)))
        (when expansion-pair
          (pcase-let ((`(,prefix . ,expansion-list) expansion-pair))
            (let ((expansion (car expansion-list)))
              (setq mono-complete--preview-overlay
                    (mono-complete--preview-create-overlay prefix expansion)))))))))


;; ---------------------------------------------------------------------------
;; Internal Hooks

(defun mono-complete--pre-command-hook ()
  "Function run from `pre-command-hook'."
  (declare (important-return-value nil))
  (unless mono-complete--suppress-command-hooks
    (cond
     (mono-complete--preview-overlay
      (setq mono-complete--preview-overlay-was-visible (mono-complete--preview-state-from-overlay))
      (delete-overlay mono-complete--preview-overlay)
      (setq mono-complete--preview-overlay nil))
     (t
      (setq mono-complete--preview-overlay-was-visible nil)))))

(defun mono-complete--post-command-hook ()
  "Function run from `post-command-hook'."
  (declare (important-return-value nil))
  (unless mono-complete--suppress-command-hooks
    (let ((do-reset :unset)
          (do-clear-timer t))

      (when (mono-complete--is-self-insert-command this-command)
        (cond
         ((mono-complete--preview-refresh-from-state mono-complete--preview-overlay-was-visible)
          (mono-complete--debug-log "idle-timer: no-reset, use overlay in-place.")
          (setq do-reset nil))
         (t
          ;; Keep cache when inserting text,
          ;; each completion must choose if cache should be reused or not.
          (when mono-complete--preview-overlay-was-visible
            (setq do-reset nil))

          (cond
           (mono-complete--preview-timer
            (mono-complete--debug-log "idle-timer: reuse (reset time).")
            (timer-set-idle-time mono-complete--preview-timer mono-complete-preview-delay nil))
           (t
            (mono-complete--debug-log "idle-timer: create.")
            (setq mono-complete--preview-timer
                  (run-with-idle-timer mono-complete-preview-delay nil #'mono-complete--preview
                                       (current-buffer)))))

          (setq do-clear-timer nil))))

      (when (eq do-reset :unset)
        (setq do-reset (not (mono-complete--is-mono-complete-command this-command))))

      (when do-clear-timer
        (when (timerp mono-complete--preview-timer)
          (cancel-timer mono-complete--preview-timer)
          (setq mono-complete--preview-timer nil)))

      (when do-reset
        (mono-complete--on-exit)))))


;; ---------------------------------------------------------------------------
;; Internal Mode Management

;; Temporary enable/disable are used so modal editing can temporarily
;; enable mono-complete functionality while in "insert" mode.
;; This has the advantage that it can be disabled unless inserting text.

(defun mono-complete--temporary-enable ()
  "Temporary enable."
  ;; Only add hooks.
  (mono-complete--command-hooks-enable))

(defun mono-complete--temporary-disable ()
  "Temporary disable."
  (mono-complete--command-hooks-disable)
  ;; Clear overlays.
  (mono-complete--on-exit)

  (when (timerp mono-complete--preview-timer)
    (cancel-timer mono-complete--preview-timer)
    (setq mono-complete--preview-timer nil)))

(defun mono-complete--command-hooks-enable ()
  "Enable command hooks."
  (declare (important-return-value nil))
  (add-hook 'pre-command-hook #'mono-complete--pre-command-hook nil t)
  (add-hook 'post-command-hook #'mono-complete--post-command-hook nil t))

(defun mono-complete--command-hooks-disable ()
  "Disable command hooks."
  (declare (important-return-value nil))
  (remove-hook 'pre-command-hook #'mono-complete--pre-command-hook t)
  (remove-hook 'post-command-hook #'mono-complete--post-command-hook t))

(defun mono-complete--mode-enable ()
  "Turn on option `mono-complete-mode' for the current buffer."
  (declare (important-return-value nil))

  (let ((enable-handled nil))
    (cond
     ;; It's possible evil mode variables are available
     ;; but the user is not in evil mode for this buffer, so check `evil-mode' first.
     ((and mono-complete-evil-insert-mode-only (bound-and-true-p evil-mode))
      (add-hook 'evil-insert-state-entry-hook #'mono-complete--temporary-enable nil t)
      (add-hook 'evil-insert-state-exit-hook #'mono-complete--temporary-disable nil t)

      ;; The symbol `evil-state' is most likely set when `evil-mode' is enabled.
      ;; Check it's bound to avoid an error in the off-chance it's not.
      (when (and (boundp 'evil-state) (memq (symbol-value 'evil-state) (list 'replace 'insert)))
        (mono-complete--command-hooks-enable))
      (setq enable-handled t))

     ;; Same as evil mode logic.
     ((and mono-complete-meow-insert-mode-only (bound-and-true-p meow-global-mode))
      (add-hook 'meow-insert-enter-hook #'mono-complete--temporary-enable nil t)
      (add-hook 'meow-insert-exit-hook #'mono-complete--temporary-disable nil t)
      (when (and (fboundp 'meow--current-state) (eq (meow--current-state) 'insert))
        (mono-complete--command-hooks-enable))
      (setq enable-handled t))
     ;; Generic insert mode support.
     (mono-complete-generic-insert-mode-functions
      (let ((is-insert-mode nil))
        (with-demoted-errors "mono-complete: generic-insert mode error (%S)"
          (pcase-let ((`(,generic-predicate-fn ,generic-enter-hook ,generic-exit-hook)
                       mono-complete-generic-insert-mode-functions))
            (add-hook generic-enter-hook #'mono-complete--temporary-enable nil t)
            (add-hook generic-exit-hook #'mono-complete--temporary-disable nil t)
            (setq is-insert-mode (funcall generic-predicate-fn))))

        (when is-insert-mode
          (mono-complete--command-hooks-enable))
        (setq enable-handled t))))

    (unless enable-handled
      (mono-complete--command-hooks-enable)))

  ;; Run `setup' on all back-ends.
  (let ((backends (mono-complete--backends-from-config nil)))
    (while backends
      (let ((backend-item (pop backends)))
        (when backend-item
          (pcase-let ((`(,config ,setup-fn ,_prefix-fn ,_complete-fn)
                       (mono-complete--backend-items-or-warn backend-item)))
            (when setup-fn
              (let ((config-next
                     (condition-case-unless-debug err
                         (funcall setup-fn config)
                       (error
                        (message "mono-complete: setup %S error (%S)" setup-fn err)
                        ;; Skip the back-end.
                        t))))
                (plist-put backend-item :config config-next)))))))))

(defun mono-complete--mode-disable ()
  "Turn off option `mono-complete-mode' for the current buffer."
  (declare (important-return-value nil))
  (mono-complete--on-exit)

  (mono-complete--command-hooks-disable)

  (when mono-complete-evil-insert-mode-only
    ;; Harmless if these were not added.
    (remove-hook 'evil-insert-state-entry-hook #'mono-complete--temporary-enable t)
    (remove-hook 'evil-insert-state-exit-hook #'mono-complete--temporary-disable t))

  (when mono-complete-meow-insert-mode-only
    ;; Harmless if these were not added.
    (remove-hook 'meow-insert-enter-hook #'mono-complete--temporary-enable t)
    (remove-hook 'meow-insert-exit-hook #'mono-complete--temporary-disable t))

  (when mono-complete-generic-insert-mode-functions
    (with-demoted-errors "mono-complete: generic-insert mode error (%S)"
      (pcase-let ((`(,_generic-predicate-fn ,generic-enter-hook ,generic-exit-hook)
                   mono-complete-generic-insert-mode-functions))
        (remove-hook generic-enter-hook #'mono-complete--temporary-enable t)
        (remove-hook generic-exit-hook #'mono-complete--temporary-disable t))))

  (when mono-complete--preview-overlay
    (delete-overlay mono-complete--preview-overlay))
  (when mono-complete--preview-timer
    (cancel-timer mono-complete--preview-timer))

  (kill-local-variable 'mono-complete--preview-overlay)
  (kill-local-variable 'mono-complete--preview-overlay-was-visible)
  (kill-local-variable 'mono-complete--preview-timer)
  (kill-local-variable 'mono-complete--context))

(defun mono-complete--expand-impl ()
  "Expand the completion, return non-nil on success."
  (declare (important-return-value nil))
  (let ((text (mono-complete--preview-text-from-command)))
    (when (string-empty-p text)
      (setq text nil))

    (cond
     (text
      (cond
       (mono-complete-literal-input
        (let ((mono-complete--suppress-command-hooks t))
          (mono-complete--insert-with-literal-input text)))
       (t
        (insert text)))

      ;; This would be called anyway in the post-command hook,
      ;; nevertheless, call early as this is known to be invalid at this point.
      (mono-complete--on-exit)

      t)

     (t
      nil))))


;; ---------------------------------------------------------------------------
;; Public API

;;;###autoload
(defun mono-complete-expand ()
  "Expand the completion, return non-nil on success."
  (declare (important-return-value nil))
  (interactive)
  (when (mono-complete--interactive-or-non-literal-input)
    (mono-complete--expand-impl)))

;;;###autoload
(defun mono-complete-expand-or-fallback ()
  "Expand the completion, return non-nil on success.
Otherwise run `mono-complete-callback-fn' and return it's result."
  (declare (important-return-value nil))
  (interactive)
  (when (mono-complete--interactive-or-non-literal-input)
    (let ((result (mono-complete--expand-impl)))
      (cond
       (result
        result)
       (t
        (cond
         (mono-complete-literal-input
          (let ((mono-complete--suppress-command-hooks t))
            (mono-complete--call-interactively-macro mono-complete-fallback-command)))
         (t
          (call-interactively mono-complete-fallback-command))))))))

;;;###autoload
(define-minor-mode mono-complete-mode
  "Enable enhanced compilation."
  :global nil

  (cond
   (mono-complete-mode
    (mono-complete--mode-enable))
   (t
    (mono-complete--mode-disable))))

(provide 'mono-complete)
;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; elisp-autofmt-format-quoted: nil
;; End:
;;; mono-complete.el ends here
