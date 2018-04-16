;;; setup-company-mode --- Customizations for company mode

;;; Commentary:

;;; Code:

;; I tried the auto-complete package and tried to get jedi working
;; with it however I was unable to. Instead I am going to try company
;; mode.

;(use-package auto-complete
;  :ensure t)

(use-package company
  :ensure t
  :diminish "Cmp"
  :config

  ;; Disable company mode for certain modes.
  (setq company-global-modes '(not org-mode
                                   gfm-mode
                                   markdown-mode
                                   eshell-mode
                                   shell-mode
                                   term-mode))

  ;; Make completion start immediately instead
  ;;of after waiting for 3 chars or half a second.
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0)

  ;; Use company mode in all buffers.
  (add-hook 'after-init-hook 'global-company-mode))

;; The company quickhelp mode was not very nice.
;; (use-package company-quickhelp
;;  :ensure t)
;;
;; Turn on the quickhelp in all buffers.
;; (company-quickhelp-mode 1)

;; M-x company-complete runs the completion manually. You can check the dropdown
;; to see if it contains the correct items.

;; Right now I have company working in elisp files.

;; References
;; http://company-mode.github.io/

(provide 'setup-company-mode)
;;; setup-company-mode ends here
