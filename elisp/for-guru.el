;;;
;;; Customizations for guru mode
;;;

;; Guru mode disables the arrow keys among other things.
;; https://github.com/bbatsov/guru-mode

(use-package guru-mode
  :ensure t
  :config
  (guru-global-mode +1))
