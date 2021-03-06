;;; setup-dumbjump --- Jump to definitions

;;; Commentary:

;;; Code:

(use-package dumb-jump
  :config
  (setq dumb-jump-selector 'helm) ;; (setq dumb-jump-selector 'ivy)
  (global-set-key (kbd "M-g o") 'dumb-jump-go-other-window)
  (global-set-key (kbd "M-g j") 'dumb-jump-go)
  (global-set-key (kbd "M-g b") 'dumb-jump-back)
  (global-set-key (kbd "M-g i") 'dumb-jump-go-prompt)
  (global-set-key (kbd "s-j") 'dumb-jump-go)
  (global-set-key (kbd "s-b") 'dump-jump-back)
  (global-set-key (kbd "s-o") 'dumb-jump-go-other-window)
  :ensure)

(provide 'setup-dumbjump)
;;; setup-dumbjump ends here
