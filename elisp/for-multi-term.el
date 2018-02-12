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
    (multi-term)))

(defun multi-term-right ()
  (interactive)
  (progn
    (split-window-right)
    (other-window 1)
    (multi-term)))

(use-package multi-term
  :ensure
  :config
  (setq multi-term-program "/bin/bash")
  (global-set-key (kbd "C-c t") 'multi-term-next)
  (global-set-key (kbd "C-c T") 'multi-term)
  (global-set-key (kbd "C-c _") 'multi-term-below)
  (global-set-key (kbd "C-c -") 'multi-term-below)
  (global-set-key (kbd "C-c |") 'multi-term-right))

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
