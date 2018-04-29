;;; setup-yaml --- Customization for yaml

;;; Commentary:

;;; Code:

(use-package yaml-mode
  :ensure t
  :config
  (add-hook 'yaml-mode-hook (lambda ()
                              (flycheck-select-checker 'yaml-jsyaml))))
(provide 'setup-yaml)
;;; setup-yaml ends here
