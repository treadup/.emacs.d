;;;
;;; Customizations for Python
;;;

;;
;; Jedi
;;

;; I tried using jedi for auto-complete first but I could not get it
;; to work. Switched to company and company jedi instead.

(use-package company-jedi
  :ensure t)

(defun personal-python-mode-setup ()
  (add-to-list 'company-backends 'company-jedi))

(add-hook 'python-mode-hook 'personal-python-mode-setup)
