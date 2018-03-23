;;;
;;; Customization for js2-mode
;;;

;; https://github.com/mooz/js2-mode

(use-package js2-mode
  :ensure t
  :config

  ;; Use the jsx mode for .js and React .jsx files.
  (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js2-jsx-mode))
  (add-to-list 'interpreter-mode-alist '("node" . js2-jsx-mode)))

;; Support for .js files.
;; (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;; (add-to-list 'interpreter-mode-alist '("node" . js2-mode))


;; Use company-tern for completion in JavaScript modes.
(use-package company-tern
  :ensure t)

(add-to-list 'company-backends 'company-tern)
