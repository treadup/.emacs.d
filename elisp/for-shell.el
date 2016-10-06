;; Create keybindings for the different terminal alternatives.

(defun shell-below ()
  (progn (split-window-below) (other-window 1) (shell)))

(defun term-below ()
  (progn (split-window-below) (other-window 1) (term "/bin/bash")))

(defun eshell-below ()
  (progn (split-window-below) (other-window 1) (eshell)))

(global-set-key (kbd "C-c s") (lambda () (interactive) (shell-below)))
(global-set-key (kbd "C-c t") (lambda () (interactive) (term-below)))
(global-set-key (kbd "C-c e") (lambda () (interactive) (eshell-below)))


