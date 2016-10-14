;;;
;;; Customizations for Auto Complete
;;;

;; I tried the auto-complete package and tried to get jedi working
;; with it however I was unable to. Instead I am going to try company
;; mode.

;(use-package auto-complete
;  :ensure t)

(defun personal-completion-setup ()
  (add-hook 'prog-mode-hook 'company-mode))
  
(use-package company
  :ensure t
  :config
  (personal-completion-setup))

(use-package company-quickhelp
  :ensure t
  :config
  (company-quickhelp-mode 1))

