;;; setup-shell --- Customizations for shell.

;;; Commentary:

;;; Code:

;; Use bash when starting a normal shell.
(customize-set-variable 'explicit-shell-file-name "/bin/bash")
(customize-set-variable 'shell-file-name "/bin/bash")

;; Create keybindings for the different terminal alternatives.

(defun shell-below ()
  (interactive)
  (progn (split-window-below) (other-window 1) (shell)))

(defun term-below ()
  (interactive)
  (progn (split-window-below) (other-window 1) (term "/bin/bash")))

(defun ielm-below ()
  (interactive)
  (progn (split-window-below) (other-window 1) (ielm)))

;; These keybindings conflict with org mode.
; (global-set-key (kbd "C-c s") (lambda () (interactive) (shell-below)))
; (global-set-key (kbd "C-c t") (lambda () (interactive) (term-below)))
; (global-set-key (kbd "C-c e") (lambda () (interactive) (eshell-below)))
; (global-set-key (kbd "C-c i") (lambda () (interactive) (ielm-below)))

(defun create-shell ()
  "Create a shell with a given name."
  (interactive);; "Prompt\n shell name:")
  (let ((shell-name (read-string "shell name: " nil)))
    (shell (concat "*" shell-name "*"))))

;; http://emacsredux.com/blog/2013/03/29/terminal-at-your-fingertips/
(defun visit-term-buffer ()
  "Create or visit a terminal buffer."
  (interactive)
  (if (not (get-buffer "*ansi-term*"))
    (progn
      (split-window-sensibly (selected-window))
      (other-window 1)
      (ansi-term (getenv "SHELL")))
    (switch-to-buffer-other-window "*ansi-term*")))

(defun create-term ()
  "creates a term with a given name"
  (interactive);; "Prompt\n shell name:")
  (let ((term-name (read-string "term name: " nil)))
    (term (concat "*" term-name "*"))))

(defun oak-shell ()
  (interactive)
  (let ((default-directory "/ssh:henrik@oak:"))
    (create-shell)))
