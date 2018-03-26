;;;
;;; Customizations for Ido mode
;;;

;; Ido mode allows you to more easily navigate choices.
; (ido-mode t)

;; Use partial matching
; (setq ido-enable-flex-matching t)

;; Enable ido wherever it could be useful.
; (setq ido-everywhere t)

;; Change the way the list of buffers is shown.
;(global-set-key (kbd "C-x C-b") 'ibuffer)

;; The smex package provides an Ido like interface for M-x.
;;(use-package smex
;;  :ensure t
;;  :bind (("M-x" . smex))
;;  :config (smex-initialize))

;;
;; I am not sure if you have to install both ivy and counsel or if
;; counsel pulls in ivy automatically.

;;(use-package counsel
;;  :ensure t)

;; For ido when in find file press C-f to go back to the old behvior.
;; https://stackoverflow.com/questions/17986194/emacs-disable-automatic-file-search-in-ido-mode
