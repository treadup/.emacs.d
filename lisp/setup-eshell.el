;;; setup-eshell --- Customizations for Eshell.

;;; Commentary:

;; Allow eshell to modify the global environment.  This is needed so that when
;; we switch python virutal environments this change also shows up in Eshell.
;; (setq eshell-modify-global-environment t)

;; See if I can't define my own methods for handling virutal environments
;; in eshell.
;;
;; workon - activate a virtual environment
;; deactivate - deactivate the current virtual environment
;; mkvenv
;; rmvenv
;; lsvenv

;; These could work by managing the eshell-path-env ourselves.  When we call workon
;; we would append the virtual environments bin folder to custom-eshell-path-env.
;; Then in the custom-eshell-prompt-function we could update eshell-path-env from
;; custom-eshell-path-env.  This will work but it might be slow.

;;; Code:

(require 'eshell)

;; Since we are inside Emacs we do not want to use a pager. Use cat as a kind of
;; no operation pager.
(setenv "PAGER" "cat")

(defvar custom-eshell-path-env)
(setq custom-eshell-path-env eshell-path-env)

;; This is a hack to get Eshell to respect the bin folder from
;; the Python virtual environment.
(defun custom-eshell-mode-hook ()
"Set the eshell-path-env to the current custom version.
The custom-eshell-path-env is updated whenever you switch virtual
environments."
  (setq eshell-path-env custom-eshell-path-env)

  ;; Create aliases
  (eshell/alias "ff" "find-file $1")
  (eshell/alias "emacs" "find-file $1")

  ;; Since we are using xterm-color.el we can have a terminal
  ;; with 256 colors instead of a dumb terminal.
  (setenv "TERM" "xterm-256color"))

(add-hook 'eshell-mode-hook 'custom-eshell-mode-hook)

;; The quit function allows us to execute the quit command
;; in eshell to close the eshell buffer and window.
;; Remember that in eshell you can run functions without
;; using parenthesis.

(defun eshell/quit ()
"Kill the current buffer and window by typing quit in eshell.
This might be better as an Eshell alias."
  (kill-buffer-and-window))

;; (eshell t) will create a new eshell with the next eshell index number.

;; This one is kind of interesting.
;; (eshell <num>) will do one of two things.
;; If an eshell with index number <num> already exists then switch the
;; window to that buffer.
;; Otherwise create a new eshell with the given index number and show
;; it in the current window.

(defun eshell-below ()
"Open an eshell in a new window below the current window."
  (interactive)
  (progn
    (split-window-below)
    (other-window 1)
    (eshell t)
    (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)))

(defun eshell-right ()
"Open an eshell in a new window to the right og the current window."
  (interactive)
  (progn
    (split-window-right)
    (other-window 1)
    (eshell t)
    (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)))

;;
;; Custom prompt
;;

;; TODO: It should be possible to have shortended paths in Eshell just like
;; we have in fish.
;; https://www.emacswiki.org/emacs/EshellPrompt

(defun with-color (text color)
"Set the foreground color of the given TEXT to COLOR."
  (propertize text 'face `(:foreground ,color)))

(defun custom-eshell-prompt-virtualenv ()
"The following will find the name of the current virtual environment.
If there is no current virtual environment return a blank string."
  (let ((venv-path (vpy-current-venv)))
    (if (s-blank? venv-path)
      ""
      (with-color
        (concat (car (last (s-split "/" venv-path))) " ")
        "cyan"))))

(defun custom-eshell-prompt-location ()
"Return user@hostname."
  (with-color
    (concat (user-login-name) "@" (hostname) " ")
    "green"))

(defun custom-eshell-prompt-git-branch ()
  "Return the current git branch."
  (with-color
    (concat (magit-get-current-branch) " ")
    "magenta"))

(defun custom-eshell-prompt-path ()
  "Return the current path."
  (with-color
    (abbreviate-file-name (eshell/pwd))
    "lightGrey"))

(defun custom-eshell-prompt-char ()
"Return the prompt character.
For non root users this is $.  For the root user this is #."
  (with-color
    (if (= (user-uid) 0) "\n# " "\n$ ")
    "lightGrey"))


;; The next issue is if there is a getenv function that gets the value from the local
;; environment. I.e. gets the value from the remote server when you are logged into
;; a remote server?

(defun custom-eshell-prompt-function ()
  "Custom Eshell prompt."
  (concat
    (custom-eshell-prompt-virtualenv)
    (custom-eshell-prompt-location)
    (custom-eshell-prompt-git-branch)
    (custom-eshell-prompt-path)
    (custom-eshell-prompt-char)))

;; Use customize-set-variable instead of setq when setting variables created
;; by defcustom.
(customize-set-variable 'eshell-prompt-function 'custom-eshell-prompt-function)

;; If we have a multiline prompt then the regexp only needs to match the last
;; line in the prompt.
(customize-set-variable 'eshell-prompt-regexp "[$#] ")

(provide 'setup-eshell)
;;; setup-eshell.el ends here
