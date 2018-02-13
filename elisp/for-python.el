;;; for-python --- Customizations for Python
;;; Commentary:
;;;

;; TODO:
;; 1. Virtual environments in Emacs
;;    Want them to work in the python shell and eshell.
;;    Use named virtual environments and make sure that it is
;;    compatible with pew.
;; 2. Auto completion using company
;; 3. Inferior shell using ipython
;; 4. Fix the mode line so that it contains the name of the
;;    virtual environment.

;;; Code:
(use-package anaconda-mode
  :ensure t
  :config
  (progn
    ;; Activate anaconda-mode for all Python buffers.
    (add-hook 'python-mode-hook 'anaconda-mode)

    ;; Anaconda has an eldoc mode. See if I can get this working.
    ;; TODO: I think that 'anaconda-eldoc-mode is a free variable here.
    (add-hook 'python-mode-hook 'anaconda-eldoc-mode)

    ;; Add company-anaconda as a company backend.
    ;; This should work since we use ensure for both the python mode
    ;; and for company mode and company mode comes before python
    ;; mode in the init file.
;;    (eval-after-load "company"
    ;;      '(add-to-list 'company-backends 'company-anaconda)))
    ))

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
(use-package company-anaconda
  :ensure t
  :config
  (setq-default company-backends (list 'company-anaconda)))

; (add-to-list 'company-backends 'company-anaconda)

;; Add company-anaconda as a company backend.
;; (eval-after-load "company"
;;  '(add-to-list 'company-backends 'company-anaconda))

;; I might want to think about using pyenv mode.
;; https://github.com/proofit404/pyenv-mode
