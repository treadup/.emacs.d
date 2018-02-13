;;;
;;; Customizations for multi-term
;;;

;; See if there are any differences in how bash, fish and zsh are supported.

(defun delete-all-windows-for-buffer (buffer)
  (dolist (window (get-buffer-window-list (buffer))
            (delete-window window))))

(defun multi-term-below ()
  (interactive)
  (progn
    (split-window-below)
    (other-window 1)
    (multi-term)
    (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)))

(defun multi-term-right ()
  (interactive)
  (progn
    (split-window-right)
    (other-window 1)
    (multi-term)
    (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)))

(defun multi-term-current-buffer ()
  (interactive)
  (progn
    (multi-term)
    (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)))

(use-package multi-term
  :ensure
  :config
  (setq multi-term-program "/bin/bash")
  (global-set-key (kbd "C-c t") 'multi-term-next)
  (global-set-key (kbd "C-c T") 'multi-term)
  (global-set-key (kbd "C-c _") 'multi-term-below)
  (global-set-key (kbd "C-c -") 'multi-term-below)
  (global-set-key (kbd "C-c |") 'multi-term-right)
  (global-set-key (kbd "C-c .") 'multi-term-current-buffer))

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


;; http://emacs-fu.blogspot.se/2010/06/console-apps-in-emacs-with-multi-term.html
;; http://rawsyntax.com/blog/learn-emacs-zsh-and-multi-term/
;; https://emacs.stackexchange.com/questions/2146/how-to-open-and-rename-several-multi-term-buffers-on-start-up
;; https://www.emacswiki.org/emacs/MultiTerm
;; https://medium.com/@lukaszkorecki/emacs-as-tmux-replacement-2acd10d7dfc8
