;;;
;;; Customizations for Python
;;;

(use-package anaconda-mode
  :ensure t)

;; Activate anaconda-mode for all Python buffers.
(add-hook 'python-mode-hook 'anaconda-mode)

;; Anaconda has an eldoc mode. See if I can get this working.
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)

;;
;; Jedi
;;

;; I tried using jedi for auto-complete first but I could not get it
;; to work. Switched to company and company jedi instead.

;(use-package company-jedi
;  :ensure t)

;(defun personal-python-mode-setup ()
;  (add-to-list 'company-backends 'company-jedi))

;(add-hook 'python-mode-hook 'personal-python-mode-setup)
