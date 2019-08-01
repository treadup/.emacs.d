;;; setup-rust --- Customization for rust.el

;;; Commentary:

;;; Code:

(use-package rust-mode
  :ensure t)

(use-package cargo
  :ensure t
  :config
  (add-hook 'rust-mode-hook 'cargo-minor-mode))

(provide 'setup-rust)
;;; setup-rust.el ends here
