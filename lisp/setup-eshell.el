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

;; We want to enable Eshell smart display mode. This gives us a built in pager for
;; all commands. Space will move down a page. Backspace will move back a page.
;; https://www.masteringemacs.org/article/complete-guide-mastering-eshell

;;; Code:

(require 'eshell)

;; Use Eshell smart display
(require 'em-smart)

;; Configure Eshell smart display
(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)

;; Since we are inside Emacs we do not want to use a pager. Use cat as a kind of
;; no operation pager.
(setenv "PAGER" "cat")   ;; TODO: Might want to test to set the pager to the empty string "".

;; I think that the custom-eshell-path-env is deprecated. Since I switched to
;; using vpy there is no reason to use this anymore I think.
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
  (eshell/alias "ll" "ls -l $*")
  (eshell/alias "e" "find-file $1")
  (eshell/alias "emacs" "find-file $1")

  ;; Enable Eshell smart display.
  (eshell-smart-initialize)

  ;; Since we are using xterm-color.el we can have a terminal
  ;; with 256 colors instead of a dumb terminal.
  (setenv "TERM" "xterm-256color")

  ;; Disable current line highlighting.
  (setq-local global-hl-line-mode nil)

  ;; Disable yasnippet mode
  (yas-minor-mode -1))

(add-hook 'eshell-mode-hook 'custom-eshell-mode-hook)

(defun custom-eshell-exec-hook (executing-process)
  "Do not query when killing an Eshell currently running a process.
The EXECUTING-PROCESS is the process being executed by Eshell."
  (tramp-compat-set-process-query-on-exit-flag executing-process nil))

(add-hook 'eshell-exec-hook 'custom-eshell-exec-hook)

(defun eshell/lcd (&optional directory)
  "Change directory to DIRECTORY.
If no directory argument is given go to the home folder
on the current machine."
  (if (and (null directory) (file-remote-p default-directory))
    (eshell/cd (file-remote-p default-directory))
    (eshell/cd directory)))

;;
;; Buffer management
;;

;; (eshell t) will create a new eshell with the next eshell index number.

;; This one is kind of interesting.
;; (eshell <num>) will do one of two things.
;; If an eshell with index number <num> already exists then switch the
;; window to that buffer.
;; Otherwise create a new eshell with the given index number and show
;; it in the current window.

(defun esh ()
  "Open a new Eshell buffer in the current window."
  (interactive)
  (eshell t))

(defun esh-below ()
"Open an eshell in a new window below the current window."
  (interactive)
  (progn
    (split-window-below)
    (other-window 1)
    (esh)))

(defun esh-right ()
"Open an eshell in a new window to the right of the current window."
  (interactive)
  (progn
    (split-window-right)
    (other-window 1)
    (esh)))

;;
;; Keybindings for Eshell
;;

;; (global-set-key (kbd "C-c t") 'multi-term-next)
(global-set-key (kbd "C-c _") 'esh-below)
(global-set-key (kbd "C-c -") 'esh-below)
(global-set-key (kbd "C-c |") 'esh-right)
(global-set-key (kbd "C-c .") 'esh)
(global-set-key (kbd "<f1>") 'esh)

;;
;; Custom prompt
;;

(defun shorten-directory-component (directory-component)
"Shorten the DIRECTORY-COMPONENT.
The shortened version consists of a string containing the first
character of the DIRECTORY-COMPONENT.  If the first character is
a period then the shortened version consists of the first two
characters in the DIRECTORY-COMPONENT."
  (if (null directory-component) ""
    (if (string-empty-p directory-component) ""
      (let ((first-char (elt directory-component 0)))
        (if (= first-char ?.)
          (s-prepend "." (shorten-directory-component (substring directory-component 1)))
          (string first-char))))))

(defun shorten-directory (directory)
"Shorten the DIRECTORY name.
Replace ancestor directory names with the first character in the directory name.
If on of the ancestor directory names starts with a period replace it with the
first two characters in the directory name."
  (let ((dir-name (abbreviate-file-name (directory-file-name directory))))
    (s-join "/" (reverse
                  (let ((components (reverse (s-split "/" dir-name))))
                    (cons (car components)
                      (mapcar 'shorten-directory-component (cdr components))))))))

(defun remote-shorten-directory (directory)
  "Shorten the DIRECTORY name which can be a local or remote directory name."
  (s-join ":" (reverse
    (let ((components (reverse (s-split ":" directory))))
    (cons (shorten-directory (car components))
      (cdr components))))))

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

;; In tramp we can have multi hops. This kind of means that the host
;; and path get intermingled. Perhaps it is best to skip the idea of
;; having the location and path be separate things in the prompt?

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
    (remote-shorten-directory (eshell/pwd))
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
