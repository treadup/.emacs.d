;;; setup-web-mode --- Customizations for web mode

;;; Commentary:
;; The web-mode website is located at the following URL.
;; http://web-mode.org/

;; The emmet-mode website can be found at the following URL.
;; https://github.com/smihica/emmet-mode

;;; Code:

(use-package web-mode
  :ensure t
  :config
  (setq web-mode-engines-alist '(("django"    . "\\.html\\'")))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

  ;; Use web-mode for JSX files.
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode)))

(use-package emmet-mode
  :ensure t
  :config
  ;; Auto start emmet-mode on any SGML like markup modes.
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'html-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook  'emmet-mode)
  (add-hook 'web-mode-hook  'emmet-mode))

(provide 'setup-web-mode)
;;; setup-web-mode ends here
