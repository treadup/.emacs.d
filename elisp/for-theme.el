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
