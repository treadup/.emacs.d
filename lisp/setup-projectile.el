;;; setup-projectile --- Customizations for projectile

;;; Commentary:

;;; Code:

;; Projectile is a project interaction library for Emacs.
;; It provides functions for project navigation.
;; https://www.emacswiki.org/emacs/Projectile
(use-package projectile
  :ensure t
  :config

  ;; Should create a hydra to use as the projectile-switch-project-action.
  ;; For now I will use projectile commander.
  (setq projectile-switch-project-action #'projectile-commander)

  ;; Set the modeline indicator to Prj[<project name>]
  (customize-set-variable 'projectile-mode-line
    '(:eval (format "Prj[%s]" (projectile-project-name))))

  ;; Enable projectile everywhere
  (projectile-mode))

(provide 'setup-projectile)
;;; setup-projectile ends here
