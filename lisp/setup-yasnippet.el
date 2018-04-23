;;; setup-yasnippet --- Customizations for yasnippet

;;; Commentary:

;; https://github.com/joaotavora/yasnippet
;; https://melpa.org/#/yasnippet
;; http://wikemacs.org/wiki/Yasnippet

;; The following link contains the Yasnippet documentation.
;; http://joaotavora.github.io/yasnippet/

;;;
;;; Organization
;;;

;; Snippet collections are stored in plain text files. There is a top
;; level directory that contains subdirectories. These subdirectories
;; contain snippet files. Each subdirectory defines a snippet table.
;; The snippets in the snippet table are defined by the snippet files
;; in the subdirectory. Snippet table names usually correspond to
;; Emacs major mode names. This way we can get automatic loading of
;; snippets based on which major mode we are in.

;; A snippet collection is a directory that contains subdirectories
;; (snippet tables) that in turn contain snippets. You can have
;; multiple snippet collections.

;; The yas-snippet-dirs should be a list of top level snippet
;; collection directories. Currently it contains the following
;; directory names. .emacs.d/snippets - This is where I should store
;; personal snippets. yas-installed-snippets-dir and
;; yasnippet-snippets-dir which contains the snippets that come
;; bundled with the yasnippet and yasnippet-snippets packages.

;; It is useful for certain modes to share snippets between each
;; other. In yasnippet it is possible for a mode to have a parent
;; mode. Another way of saying this is that a snippet table can have a
;; parent snippet table. A mode can have multiple parent modes. It is
;; possible for a mode to have three or more parent modes. It is
;; probably better to think of parent modes as modes that are included
;; in the current mode.

;; For any given mode/snippet table folder you can specify the parent
;; modes using a .yas-parents file. This file contains a space
;; separated list of the parent mode names.

;; Each snippet table directory can contain a .yas-setup.el file.
;; Custom code and functions that are used by the snippets can go
;; here.

;;;
;;; Expanding Snippets
;;;

;; The yas-expand function tries to expand a snippet abbrev (also
;; known as a snippet key) before point. Yasnippet also provides a
;; conditional bidning for this command. The variable yas-maybe-expand
;; contains a special value, which when bound to a keymap tells Emacs
;; to call yas-expand if and only if there is a snippet abbrev before
;; point. If there is no snippet to expand Emacs will behave as if
;; yas-expand is unbound and so will run whatever command is bound to
;; that key normally.

;; When yas-minor-mode is enabled, it binds yas-maybe-expand to TAB
;; and <tab> by default, however, you can freely remove those
;; bindings.

;; You can call M-x yas-expand manually to test out the snippets.

;; There seems to be some kind of weird interaction between python completion
;; and yasnippet. Not sure what is going on here.

;; To solve this change to using the SPC key for snippet-abbrev expansion. Should
;; work fine and there should be no interaction problems. The one thing that we
;; have to do is to disable expansion inside comments and string literals.

;; We probably want to disable yasnippet in Eshell?

;;;
;;; Editing snippets
;;;

;; The yas-new-snippet function can be used to create a new snippet.
;; The yas-visit-snippet-file function is used to visit an existing snippet file.
;; There is a major mode for editing snippets called snippet-mode. You can switch
;; a buffer to sippet mode using the M-x snippet-mode command.

;;;
;;; Snippet file format
;;;

;; Each snippet it stored in a file. The snippet file has the following format.
;; There is a special line that contains
;; # --
;; as its only contents. Lines above this line are comments. Lines below this line
;; are part of the snippet.

;; If there is no # -- line then all lines are considered to be part of the template.

;; Comment lines should start with the hash # character.

;; There is a special kind of comment line called a snippet directive. Snippet
;; directives have the following format.
;; # Directive: value

;; Snippet directives are used to tweak certain snippet properties.

;; The following snippet directives are supported.

;; key: snippet abbreviation
;; This is the most important directive. The value is the abbreviation you type just before
;; pressing the chord that runs yas-expand.

;; name: snippet name
;; This is a one line description of the snippet.

;; condition: snippet condition
;; This is a piece of Emacs-lisp code. If a snippet has a condition,
;; then it will only be expanded when the condition code evaluate to
;; some non-nil value.

;; binding: direct keybinding
;; You can use this directive to expand a snippet directly from a
;; normal Emacs keybinding. The keybinding will be registered in the
;; Emacs keymap named after the major mode the snippet is active for.

;; type: snippet or command

;; If the type directive is set to command, the body of the snippet is
;; interpreted as Lisp code to be evaluated when the snippet is triggered.
;; If the type is  snippet (the default when there is no type directive),
;; the snippet body will be parsed according to the Template Syntax,
;; described below.

;; uuid: unique identifier
;; This provides to a way to identify a snippet, independent of its
;; name. Loading a second snippet file with the same uuid would
;; replace the previous snippet.

;; contributor: snippet author
;; This is optional and has no effect on functionality.

;;;
;;; Template Syntax
;;;

;; See the following link. Specifically the section on template syntax.
;; http://joaotavora.github.io/yasnippet/snippet-development.html

;;; Code:

(use-package yasnippet
  :ensure t
  :config

  ;; Do not use the TAB key to expand snippet abbreviations.
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)

  ;; Do not use the space key to expand snippet abbreviations.
  ;; (define-key yas-minor-mode-map (kbd "SPC") yas-maybe-expand)

  ;; Use the paragraph key expand snippet abbreviations.
  (define-key yas-minor-mode-map (kbd "§") #'yas-expand)

  ;; Bind `C-c y' to `yas-expand' ONLY.
  (define-key yas-minor-mode-map (kbd "C-c y") #'yas-expand)

  ;; Do not expand abbreviations when inside a Python comment or string literal.
  (add-hook 'python-mode-hook
    (lambda ()
      (setq yas-buffer-local-condition
        '(not (python-syntax-comment-or-string-p)))))

  ;; Saved snippets should not contain a newline at the end of the file.
  (add-hook 'snippet-mode-hook
    (lambda ()
      (setq require-final-newline nil)))

  ;; Enable yasnippet everywhere.
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t)


(provide 'setup-yasnippet)
;;; setup-yasnippet ends here
