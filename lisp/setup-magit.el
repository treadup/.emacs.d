;;; setup-magit --- Customizations for Magit

;;; Commentary:

;;; Code:

;; Magit is a git frontend.
(use-package magit
  :ensure t
  :config
  (global-set-key (kbd "<f10>") 'magit-status))

(provide 'setup-magit)
;;; setup-magit.el ends here
