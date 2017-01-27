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


