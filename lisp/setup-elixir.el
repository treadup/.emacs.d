;;; setup-elixir --- Customization for Elixir

;;; Commentary:

;;; Code:

(use-package elixir-mode
  :ensure t)

(use-package alchemist
  :ensure t
  :config
  (add-to-list 'elixir-mode-hook 'alchemist-mode))

(provide 'setup-elixir)
;;; setup-elixir.el ends here
