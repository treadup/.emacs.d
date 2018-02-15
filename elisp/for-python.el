;;; for-python --- Customizations for Python
;;; Commentary:
;;;

;; TODO:
;; 1. Virtual environments in Emacs
;;    Want them to work in the python shell and eshell.
;;    Use named virtual environments and make sure that it is
;;    compatible with pew.
;; 2. Auto completion using company
;; 3. Anaconda eldoc mode. I do not believe that
;;    this is currently working. Flymake or something else
;;    seems to be indicating that there is an error with
;;    anaconda-eldoc-mode. However now it looks like it
;;    might be working again.
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
    (add-hook 'python-mode-hook 'anaconda-eldoc-mode)))

(use-package company-anaconda
  :ensure t
  :config

  ;; Add company-anaconda as a company backend.
  (eval-after-load "company"
    '(add-to-list 'company-backends 'company-anaconda)))

;; I might want to think about using pyenv mode.
;; https://github.com/proofit404/pyenv-mode

;; References
;; https://github.com/proofit404/anaconda-mode
;; https://github.com/proofit404/company-anaconda

;; The following are the built in keybindings for anaconda-mode.
;; C-M-i	anaconda-mode-complete
;; M-.	anaconda-mode-find-definitions
;; M-,	anaconda-mode-find-assignments
;; M-r	anaconda-mode-find-references
;; M-*	anaconda-mode-go-back
;; M-?	anaconda-mode-show-doc
