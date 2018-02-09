;;;
;;; Spacemacs theme
;;;

;; Use the theme from Spacemacs
(use-package spacemacs-theme
  :defer t
  :init (load-theme 'spacemacs-dark t))

;; Use the modeline from spacemacs
(use-package spaceline-config
  :ensure spaceline
  :config
  (spaceline-spacemacs-theme))

;; Use the wave separator character in the modeline instead of the arrow.
;; To get this to work you might have to execute (spaceline-compile)
(setq powerline-default-separator 'wave)
(spaceline-compile)
