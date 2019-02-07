;;; setup-flycheck --- Customization for flycheck

;;; Commentary:
;; Flycheck provides modern on the fly syntax checking.
;; http://www.flycheck.org/en/latest/

;;; Code:

(use-package flycheck
  :ensure t
  :diminish "FC"
  :config
  (setq flycheck-executable-find 'venv-find-executable)
  (global-flycheck-mode))

(provide 'setup-flycheck)
;;; setup-flycheck ends here
