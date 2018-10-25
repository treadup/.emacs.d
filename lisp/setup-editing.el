;;; setup-editing --- Configuring how to edit with Emacs.

;;; Commentary:

;; This has a really good kill line implementation.

;; https://github.com/bbatsov/crux

;; There is also mwim that provides some useful functions for moving
;; the point.
;; https://github.com/alezost/mwim.el

;;; Code:

(use-package crux
  :ensure t
  :config
  (progn
    (global-set-key (kbd "C-a") 'crux-move-beginning-of-line)
    (global-set-key (kbd "C-k") 'crux-smart-kill-line)
    ;; You almost never need to use kill-whole-line after switching
    ;; to using the whole-line-or-region package. Use C-w instead.
    (global-set-key (kbd "C-S-k") 'crux-kill-whole-line)
    (global-set-key (kbd "C-c 4 t") 'crux-transpose-windows)
    (global-set-key (kbd "C-c e") 'crux-eval-and-replace)
    (global-set-key (kbd "C-o") 'crux-smart-open-line)
    ;; The smart open line above function is almost never used.
    ;; Keep the keybinding as it is for now.
    (global-set-key (kbd "C-S-o") 'crux-smart-open-line-above)
    ))

;; Make it so that copying and cutting when no region is selected will
;; copy or cut the whole line.
;; This package interferes with kill-backward-word-or-region.
;; (use-package whole-line-or-region
;;   :ensure t
;;   :config
;;   (whole-line-or-region-global-mode))

(defun kill-backward-word-or-region ()
  "Kill backward word if no region is active. Otherwise kill the active region."
  (interactive)
  (if (region-active-p)
    (kill-region (region-beginning) (region-end))
    (backward-kill-word 1)))

(global-set-key (kbd "C-w") 'kill-backward-word-or-region)

;; Make it so that you can expand the region semantically.
(use-package expand-region
  :ensure t
  :config
  (global-set-key (kbd "C-c x") 'er/expand-region)
  (global-set-key (kbd "C-c X") 'er/contract-region))

(provide 'setup-editing)
;;; setup-editing ends here
