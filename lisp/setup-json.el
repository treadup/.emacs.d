;;; setup-json --- Customization for json-mode

;;; Commentary:

;;; Code:
(use-package json-mode
  :ensure t
  :config
  :bind
  (("C-c /" . 'json-pretty-print-buffer)))

(provide 'setup-json)
;;; setup-json.el ends here
