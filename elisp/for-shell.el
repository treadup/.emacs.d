;; Create keybindings for the different terminal alternatives.

(defun shell-below ()
  (interactive)
  (progn (split-window-below) (other-window 1) (shell)))

(defun term-below ()
  (interactive)
  (progn (split-window-below) (other-window 1) (term "/bin/bash")))

(defun eshell-below ()
  (interactive)
  (progn (split-window-below) (other-window 1) (eshell)))

(defun ielm-below ()
  (interactive)
  (progn (split-window-below) (other-window 1) (ielm)))

;; These keybindings conflict with org mode.
; (global-set-key (kbd "C-c s") (lambda () (interactive) (shell-below)))
; (global-set-key (kbd "C-c t") (lambda () (interactive) (term-below)))
; (global-set-key (kbd "C-c e") (lambda () (interactive) (eshell-below)))
; (global-set-key (kbd "C-c i") (lambda () (interactive) (ielm-below)))

(defun create-shell ()
  "creates a shell with a given name"
  (interactive);; "Prompt\n shell name:")
  (let ((shell-name (read-string "shell name: " nil)))
    (shell (concat "*" shell-name "*"))))

;; This is quite an interesting page
;; https://www.emacswiki.org/emacs/EshellMultipleEshellBuffers
;; https://github.com/DamienCassou/shell-switcher

(defun create-eshell ()
  "creates an eshell with a given name"
  (interactive);; "Prompt\n shell name:")
  (let ((eshell-name (read-string "eshell name: " nil)))
    (eshell (concat "*" eshell-name "*"))))

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

(defun oak-eshell ()
  (interactive)
  (let ((default-directory "/ssh:henrik@oak:"))
    (create-eshell)))
