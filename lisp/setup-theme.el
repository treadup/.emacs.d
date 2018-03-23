;;;
;;; Spacemacs theme
;;;

;; Use the theme from Spacemacs
;; (use-package spacemacs-theme
;;  :defer t
;;  :config
;;   (require 'spacemacs-common)
;;  (load-theme 'spacemacs-dark t))

;; There is an issue with the spacemacs theme not being installable using use-package.
;; Fixed by including a hacked version of the theme file in the elisp folder.
;; https://github.com/treadup/spacemacs-theme
(load "spacemacs-theme.el")

;; Use the modeline from spacemacs
(use-package spaceline-config
  :ensure spaceline
  :config
  (spaceline-spacemacs-theme))

;; Use the wave separator character in the modeline instead of the arrow.
;; To get this to work you might have to execute (spaceline-compile)
(setq powerline-default-separator 'wave)
(spaceline-compile)
