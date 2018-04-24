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
(use-package whole-line-or-region
  :ensure t
  :config
  (whole-line-or-region-global-mode))

(provide 'setup-editing)
;;; setup-editing ends here
