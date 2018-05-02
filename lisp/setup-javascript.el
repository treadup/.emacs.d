;;; setup-javascript --- Customizations for JavaScript

;;; Commentary:

;; Use js2-mode for plain javascript files.
;; https://github.com/mooz/js2-mode
;;
;; Use rjsx-mode for jsx files.
;; https://github.com/felipeochoa/rjsx-mode

;;; Code:
(use-package js2-mode
  :ensure t
  :config
  ;; Use the jsx mode for .js and React .jsx files.
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-to-list 'interpreter-mode-alist '("node" . js2-mode)))

(use-package rjsx-mode
  :ensure t
  :config
  ;; Use rjsx mode for React .jsx files.
  ;; TODO: Not sure we actually have to have this here.
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode)))

;; Use company-tern for completion in JavaScript modes.
(use-package company-tern
  :ensure t
  :config
  (add-to-list 'company-backends 'company-tern))

(provide 'setup-javascript)
;;; setup-javascript ends here
