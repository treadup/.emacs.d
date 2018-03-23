;;;
;;; Customizations for projectile
;;;

;; Projectile is a project interaction library for Emacs.
;; It provides functions for project navigation.
;; https://www.emacswiki.org/emacs/Projectile
(use-package projectile
  :ensure t
  :config (projectile-global-mode) ;; Enable projectile everywhere.
  :diminish "P")

;; (use-package projectile
;;  :ensure t)
