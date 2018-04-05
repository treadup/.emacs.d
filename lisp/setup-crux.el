;;; setup-crux --- Customizations for crux

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
    (global-set-key (kbd "C-k") 'crux-smart-kill-line)))

(provide 'setup-crux)
;;; setup-crux.el ends here
