;;;
;;; Customizations for Python
;;;

;;
;; Jedi
;;


(use-package company-jedi
  :ensure t)

(defun personal-python-mode-setup ()
  (add-to-list 'company-backends 'company-jedi))

;; (print company-backends)

;; (personal-python-mode-setup)

(add-hook 'python-mode-hook 'personal-python-mode-setup)
