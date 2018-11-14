;;; setup-origami --- Customization for Origami

;;; Commentary:

;; Need to come up with proper keybindings for this mode.

;;; Code:

(use-package origami
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'origami-mode))



(provide 'setup-origami)
;;; setup-origami.el ends here
