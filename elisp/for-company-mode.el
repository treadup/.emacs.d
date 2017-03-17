;;;
;;; Customizations for company mode
;;;


;; I tried the auto-complete package and tried to get jedi working
;; with it however I was unable to. Instead I am going to try company
;; mode.

;(use-package auto-complete
;  :ensure t)

(use-package company
  :ensure t)

;; Use company mode in all buffers.
(add-hook 'after-init-hook 'global-company-mode)

(use-package company-quickhelp
  :ensure t
  :config)

;; Turn on the quickhelp in all buffers.
(company-quickhelp-mode 1)

;; Make completion start immediately instead of after waiting for 3 chars or half a second.
(setq company-minimum-prefix-length 1) 
(setq company-idle-delay 0)



