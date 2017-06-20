;;; for-python --- Customizations for Python
;;; Commentary:
;;;

;;; Code:
(use-package anaconda-mode
  :ensure t)

;; Activate anaconda-mode for all Python buffers.
(add-hook 'python-mode-hook 'anaconda-mode)

;; Anaconda has an eldoc mode. See if I can get this working.
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)

;; Something is giving me completion when editing Python files.
;; I think this is just plain company-mode talking to jedi. However
;; I am not sure.

;; For now I will not use company-anaconda. The company-anaconda mode was
;; causing issues when you tried to exit emacs after editing a python file.
;; It would have several running background processes and would ask you if
;; you wanted to exit each time you tried to close emacs.

;; There is also company-anaconda that is an anaconda backend for company-mode.
;; However this did not seem to work. The completions contained the function named
;; followed by the function name in brackets. foo<foo>
; (use-package company-anaconda
;   :ensure t)

; (add-to-list 'company-backends 'company-anaconda)

;; Add company-anaconda as a company backend.
;; (eval-after-load "company"
;;  '(add-to-list 'company-backends 'company-anaconda))

;; I might want to think about using pyenv mode.
;; https://github.com/proofit404/pyenv-mode
