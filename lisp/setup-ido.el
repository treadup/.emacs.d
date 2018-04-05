;;; setup-ido --- Customizations for Ido mode

;;; Commentary:
;; For ido when in find file press C-f to go back to the old behvior.
;;
;; The ido-mode comes builtin with Emacs.  I use the following extra
;; packages with ido.
;;
;; ido-completing-read+
;; smex
;; ido-vertical-mode
;; flx-ido
;;
;;; Code:

(require 'ido)

;; Ido mode allows you to more easily navigate choices.
(ido-mode t)

;; Use partial matching
(setq ido-enable-flex-matching t)

;; Enable ido wherever it could be useful.
(setq ido-everywhere t)

;; Use ido ubiqutous mode
(use-package ido-completing-read+
  :ensure t
  :config
  (ido-ubiquitous-mode 1))

;; Change the way the list of buffers is shown.
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; The smex package provides an Ido like interface for M-x.
(use-package smex
  :ensure t
  :config (smex-initialize)
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  ;; The old M-x
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))

;; Use icomplete where ido cannot be used.
(require 'icomplete)
(icomplete-mode 1)

;; Make ido-mode display vertically.
;; https://github.com/creichert/ido-vertical-mode.el
(use-package ido-vertical-mode
  :ensure t
  :config
  (ido-vertical-mode 1)
  (setq ido-vertical-define-keys 'C-n-and-p-only))

;; Fuzzy matching for ido mode.
;; Installing flx-ido package will pull in the flx package.
;; https://github.com/lewang/flx
(use-package flx-ido
  :ensure t
  :config
  (flx-ido-mode 1)

  ;; Enable flex matching
  (setq ido-enable-flex-matching t)

  ;; Disable ido faces to see flx highlights.
  (setq ido-use-faces nil)

  ;; Set the threshold where flx-ido will switch matching algorithm.
  (customize-set-variable 'flx-ido-threshold 10000))

(provide 'setup-ido)
;;; setup-ido.el ends here
