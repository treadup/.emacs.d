;;; setup-javascript --- Customizations for JavaScript

;;; Commentary:

;; Use js2-mode for plain javascript files.
;; https://github.com/mooz/js2-mode
;;
;; The rjsx-mode might be interesting for jsx files.
;; https://github.com/felipeochoa/rjsx-mode
;;
;; For now I will use web-mode for JSX files. This is
;; configured in the setup-web-mode.el file.

;;; Code:
(use-package js2-mode
  :ensure t
  :config
  ;; Use the jsx mode for .js and React .jsx files.
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-to-list 'interpreter-mode-alist '("node" . js2-mode)))

;; Use web-mode for JSX files.
;; (use-package rjsx-mode
;;   :ensure t
;;   :config
;;   ;; Use rjsx mode for React .jsx files.
;;   ;; TODO: Not sure we actually have to have this here.
;;   (add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode)))

;; Use company-tern for completion in JavaScript modes.
(use-package company-tern
  :ensure t
  :config
  (add-to-list 'company-backends 'company-tern))

(provide 'setup-javascript)
;;; setup-javascript ends here
