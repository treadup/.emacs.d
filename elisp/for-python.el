;;; for-python --- Customizations for Python
;;; Commentary:
;;;

;; This configuration does the following.
;; 1. Activates anaconda-mode
;; 2. Set the python-shell-extra-pythonpaths to contain the root
;;    folder of the current projectile project.
;; 3. TODO: Activate the virtual environment in the .venv file in the current
;;    projectile project.
;; 3. Turn on auto-completion using company-anaconda.
;; 4. Turn on eldoc using anaconda-eldoc-mode
;; 5. Use iPython for the inferior shell
;; 6. TODO: The mode line should contain the name of the virtual environment.

;;; Code:
(use-package anaconda-mode
  :ensure t
  :config
  (progn
    ;; Activate anaconda-mode for all Python buffers.
    (add-hook 'python-mode-hook 'anaconda-mode)

    ;; Anaconda has an eldoc mode. See if I can get this working.
    (add-hook 'python-mode-hook 'anaconda-eldoc-mode)))

(use-package company-anaconda
  :ensure t
  :config

  ;; Add company-anaconda as a company backend.
  (eval-after-load "company"
    '(add-to-list 'company-backends 'company-anaconda)))

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

;; It can be a bit tricky to get anaconda-mode working.
;; I think I have installed the following packages in
;; the global environment.
;;
;;    pip install jedi==0.8.1 json-rpc==1.8.1 service_factory==0.1.2


;; Use the pyvenv package to mange virtual environments.
;; https://github.com/jorgenschaefer/pyvenv
;; This is the package that spacemacs uses to manage virtual environments.
(use-package pyvenv
  :ensure t)

;;
;; Projectile switch project hook
;;

;; There are two things that you need to configure for Python in Emacs.
;; The first is adding the root folder of the project as an extra python path.
;; The second is activating the virtual environment.

(defconst pht--original-python-extra-pythonpaths python-shell-extra-pythonpaths)

(if (projectile-project-p)
  (message (concat "Inside project" " " (projectile-project-root)))
  (message "Not in a project."))

(defun pht-activate-python-project ()
"Activate the current projectile project as a python project.
Sets the python-extra-pythonpath to the root of the project.
TODO: Activates the virtual environment."
  (if (projectile-project-p)
    (progn
      (setq python-shell-extra-pythonpaths pht--original-python-extra-pythonpaths)
      (add-to-list 'python-shell-extra-pythonpaths (projectile-project-root)))))

;; On startup activate the current projectile project, if there is
;; one, as a python project.
(pht-activate-python-project)

;; And switch python project when we switch projects in Projectile.
(add-hook 'projectile-after-switch-project-hook #'pht-activate-python-project)

;;
;; iPython
;;

;; Use ipython for the inferior shell. The --simple-prompt argument is there
;; because Emacs does not work with the default prompt in newer versions of
;; ipython.
(setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "--simple-prompt -i")
