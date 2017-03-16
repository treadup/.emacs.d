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


;; The company quickhelp mode was not very nice.
;; (use-package company-quickhelp
;;  :ensure t)
;;
;; Turn on the quickhelp in all buffers.
;; (company-quickhelp-mode 1)




