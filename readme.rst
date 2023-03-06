
###################
Emacs Mono-Complete
###################

Completion, providing a suggestion preview while typing.

The following features are supported:

- Minimal overhead (runs on idle).
- Non-blocking interaction, where any key (besides completion) will continue editing as expected.
- Input simulation. Useful when recording macros,
  so the literal text input is replayed instead of re-running the completion in a different context
  which may give a different result.
- Context sensitive, so entirely different completions can be configured based on kind of text being edited.
- Buffer local configuration. Each buffer can have it's own local completion configuration
  that may include project-local expansions.


Motivation
==========

The motivation for writing this package was to achieve something similar to completion I was used
to on the FISH-shell which can be configured to suggest something based on previously used commands.
While text entry in an editor doesn't lend it's self to command reuse in quite the same way,
an ability to complete the most "likely" candidate is still possible.

In contrast to most existing completion systems in emacs,
mono-complete focuses on providing a useful suggestion instead of a comprehensive list of candidates.
This works well with natural language input (text input typically found on phone keyboards).
Where statistical analysis can run on existing text (including code-comments, commit-logs and plain-text)
to predict the most likely words to use based on previous words while typing.

Always previewing a completion is a convenient way to show text input might not have anticipated would be available
in a way that isn't overly intrusive.

There can be multiple methods of generating suggestions which can all be enabled at once
to increase the chance of showing a useful suggestion in any context.


Usage
=====

The typical usage for this package is to enable the minor mode ``mono-complete-mode``,
then bind a key to ``mono-complete-expand`` (or ``mono-complete-expand-or-fallback``).

Aside from this you may wish to customize the available back-ends,
depending on the mode and language.


Commands
========

``mono-complete-mode``
   Enable mono-complete mode.

``mono-complete-expand``
   Expand the completion.

``mono-complete-expand-or-fallback``
   Expand the completion or run ``mono-complete-fallback-command``
   when no preview is visible.


Customization
=============

``mono-complete-backends``: ``(list 'dabbrev)``
   A list of back-ends (see `Included Backends`_). For example ``(list 'dabbrev 'filesystem 'word-predict)``.

   Completion back-ends are evaluated in-order,
   the first back-end to return a result defines the suggestion that is shown.

   Optionally this can be a function which takes a single ``is-context`` argument.
   When nil, return all backends which may be used,
   otherwise fewer backends may be returned based on the current context.

   Using a function has the advantage that the back-ends returned can be context sensitive.

   .. code-block:: elisp

      (setq mono-complete-backends
            (lambda (is-context)
              (cond
               (is-context
                (let* ((result (list))
                       (state (syntax-ppss))
                       (is-string (nth 3 state))
                       (is-comment (nth 4 state)))
                  (when (or is-string is-comment)
                    (push 'filesystem result))
                  (push 'dabbrev result)
                  result))
               (t
                (list 'dabbrev 'filesystem)))))

``mono-complete-preview-delay``: ``0.235``
   How long to wait until displaying the preview after a keystroke (in seconds).

``mono-complete-self-insert-commands``: ``'(self-insert-command org-self-insert-command)``
   A list of commands after which to show a preview.

``mono-complete-fallback-command``: ``'indent-for-tab-command``
   The command to use when ``mono-complete-expand-or-fallback`` was called without a preview.

``mono-complete-literal-input``: ``t``
   Simulate literal text input.

   When enabled replaying this action as a macro re-inserts the literal text
   instead of performing the completion action (which may give different results).

``mono-complete-evil-insert-mode-only``: ``t``
   Restrict to insert mode when used in combination with ``evil-mode``.

``mono-complete-cache-directory``
   The directory to store mono-complete cache data.

``mono-complete-preview-face``
   Face used to display the preview.


Installation
============

Here is an example using ``use-package`` & ``straight``:

.. code-block:: elisp

   (use-package mono-complete
     :config
     (setq mono-complete-fallback-command 'tab-to-tab-stop)
     (define-key mono-complete-mode-map (kbd "<tab>") 'mono-complete-expand-or-fallback)

     :commands (mono-complete-mode)
     :hook ((prog-mode) . mono-complete-mode))


Included Backends
=================

``capf``
   Complete using emacs built-in completion-at-point.

   **Customization**

   ``mono-complete-backend-capf-complete-fn``
      The completion-at-point function to use or nil to use the default (``eglot`` & ``emacs-lisp-mode`` supported).

``dabbrev``
   Complete using words in the buffer.

``filesystem``
   Complete using paths on the file-system.

``spell-fu``
   Complete the word using the ``spell-fu`` dictionary.

``whole-line``
   Complete whole lines.

``word-predict``
   Predict the next word using previous words based on a simple statistical model (N-grams).

   This handles both extracting data from source code & text files
   as well as using that data for completion.

   *Note that this depends on Python.*

   **Customization**

   ``mono-complete-backend-word-predict-input-paths-match-source``
      A list of file extensions to match as source files (only code comments are extracted).
   ``mono-complete-backend-word-predict-input-paths-match-text``
      A list of file extensions to match as regular text (all text is extracted).
   ``mono-complete-backend-word-predict-input-paths-size-limit``
      Files larger than this are skipped.
   ``mono-complete-backend-word-predict-input-paths``
      List of paths used for extracting text (an empty string uses the projects root).
   ``mono-complete-backend-word-predict-update-method``: (``when-missing``)
      Method used for validating the model, a symbol in:

      - ``when-missing``: extract data if it doesn't exist.
      - ``from-manifest``: extract data when files are outdated.

..
   Extending Backends
   ==================

   A completion back-end is a property list containing the following keys:

   :config *(optional) list*
      This is it's self a list which may be used to configure the completion.
      You can for example: multiple instantiating of the same back-end can be
      used at once with different configurations.

   :setup *(optional) function*
      Takes a single ``(config)`` argument.

      This is a function that runs when ``mono-complete-mode`` is enabled for a buffer.

      The function may manipulate ``:config`` (taking it as an argument and returning it).
      Take care to always return the ``:config`` otherwise this will clear the configuration.
      This is it's self a list which may be used to configure the completion.

      To skip the completion back end you may:

      - Return ``t`` (this silently ignores the back-end).
      - Raise an error via calling ``error`` which shows a message.
        This should be used to fail on an invalid ``:config``.

   :prefix *function*
      Takes ``()`` no arguments, returns a string or nil.

      Return text before the cursor or nil.

   :complete *function*
      Takes ``(config prefix cache)``, returns a list of strings.

      Returns a cons cell ``(result . cache)`` where the result is a list of strings
      and the cache is an implementation defined variable which can store any values
      assist in refining the completion as additional keys are entered.

      Return a list of completion text or nil to fall through to other completers.
