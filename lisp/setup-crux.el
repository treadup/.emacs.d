;;;
;;; Customizations for crux
;;;

;; This has a really good kill line implementation.

;; https://github.com/bbatsov/crux

;; There is also mwim that provides some useful functions for moving
;; the point.
;; https://github.com/alezost/mwim.el

(use-package crux
  :ensure t
  :config
  (progn
    (global-set-key (kbd "C-a") 'crux-move-beginning-of-line)))
