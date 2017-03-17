;;;
;;; Customizations for Python
;;;

(use-package anaconda-mode
  :ensure t)

;; Activate anaconda-mode for all Python buffers.
(add-hook 'python-mode-hook 'anaconda-mode)

;; Anaconda has an eldoc mode. See if I can get this working.
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)

;; Something is giving me completion when editing Python files.
;; I think this is just plain company-mode talking to jedi. However
;; I am not sure.

;; There is also company-anaconda that is an anaconda backend for company-mode.
;; However this did not seem to work. The completions contained the function named
;; followed by the function name in brackets. foo<foo>
;; (use-package company-anaconda
;;   :ensure t)

;; Add company-anaconda as a company backend.
;; (eval-after-load "company"
;;  '(add-to-list 'company-backends 'company-anaconda))

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
