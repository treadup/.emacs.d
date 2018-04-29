;;; setup-multi-term --- Customizations for multi-term

;;; Commentary:

;;; Code:

(defun multi-term-below ()
  "Open a window containing multi-term in a new window below the current one."
  (interactive)
  (progn
    (split-window-below)
    (other-window 1)
    (multi-term)
    (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)))

(defun multi-term-right ()
  "Open a window containing multi-term in a new window to the right of the current one."
  (interactive)
  (progn
    (split-window-right)
    (other-window 1)
    (multi-term)
    (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)))

(defun multi-term-current-buffer ()
  "Open multi-term in the current buffer."
  (interactive)
  (progn
    (multi-term)
    (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)))

(use-package multi-term
  :ensure
  :config
  (setq multi-term-program (executable-find "fish"))
  (unless multi-term-program
    (setq multi-term-program "/bin/bash"))
  (setq term-buffer-maximum-size 100000)
  (add-hook 'term-mode-hook
    (lambda ()
      (setq-local global-hl-line-mode nil))))

;;
;; TODO: Have some keybiding for opening a terminal in the current window.
;; Also have some keybinding for when you want to open other types of terminals.
;; It might be interesting to switch from kill-buffer to kill-buffer-and-window and
;; have it key bound to C-x k
;; Also C-c t can be the prefix for the terminal / shell menu.
;; This means that
;; C-c t e would be eshell
;; C-c t m would be multi-term

;; v - split vertically
;; h - split horizontally
;;   - take over window
;; q - kill window and buffer

;; Some functionality that is missing from Emacs is the tmux like split behavior.
;; C-c | should create a new multi-term to the right.
;; C-c _ and C-c - should create a new terminal above and below.

;; The other part of the tmux functionality that is missing is to be able to kill
;; the buffer and the window when the shell has exited.
;; There is an emacs function called kill-buffer-and-window that will let you do
;; this. Perhaps there is a way to hook it up so that it is called when you exit
;; a shell?

;; I should also be able to do all of the above using eshell.

(provide 'setup-multi-term)
;;; setup-multi-term ends here
