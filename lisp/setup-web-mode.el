;;; setup-web-mode --- Customizations for web mode

;;; Commentary:
;; The web-mode website is located at the following URL.
;; http://web-mode.org/

;; The emmet-mode website can be found at the following URL.
;; https://github.com/smihica/emmet-mode

;; We will use web-mode when editing .jsx and plain .js files.
;; The reason to use web-mode for .jsx files is that web-mode
;; is the only mode that I have found so far that handles
;; indentation of JSX correctly.

;; The reason to use web-mode for plain .js files is that
;; sometimes .js files contain JSX. If I could trust the convention
;; that .js files contained plain JavaScript and .jsx files
;; contained JSX then I could revisit this decision.

;;; Code:

(use-package web-mode
  :ensure t
  :config
  (setq web-mode-engines-alist '(("django"    . "\\.html\\'")))

  ;; Use web mode for .html and .htm files
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

  ;; Use web mode for .js and .jsx files
  (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . web-mode))

  ;; Set the content type to jsx when editing .js and .jsx files
  (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))

  ;; Indentation
  (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-quotes" . nil))
  (add-to-list 'web-mode-indentation-params '("case-extra-offset" . nil))

  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-script-padding 2)
  (setq web-mode-attr-indent-offset 2)
  (setq web-mode-enable-css-colorization t)

  ;; Disable automatic quoting
  (setq web-mode-enable-auto-quoting nil)

  ;; Highlight the current element
  (setq web-mode-enable-current-element-highlight t))

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
