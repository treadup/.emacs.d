;;;
;;; Customizations for web mode
;;;

;; The web mode website is located at the following url.
;; http://web-mode.org/

(use-package web-mode
  :ensure t
  :config
  (setq web-mode-engines-alist '(("django"    . "\\.html\\'")))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))
