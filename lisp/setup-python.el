;;; for-python --- Customizations for Python
;;; Commentary:
;;;

;; This configuration does the following.
;; 1. Activates anaconda-mode
;; 2. Set the python-shell-extra-pythonpaths to contain the root
;;    folder of the current projectile project.
;; 3. Activate the virtual environment in the .venv file in the current
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
;; Unfortunately this package does not support Eshell.
;; There is a github issue to fix this.
;; github.com/joergenschaefer/elpy/issues/1172
(use-package pyvenv
  :ensure t)

;;
;; Projectile switch project hook
;;

;; There are two things that you need to configure for Python in Emacs.
;; The first is adding the root folder of the project as an extra python path.
;; The second is activating the virtual environment.

(defconst original-python-extra-pythonpaths python-shell-extra-pythonpaths)

(defun parse-single-token (filename)
"Read a single token from the first line of the file FILENAME."
  (let ((venv-file-content (f-read-text filename)))
    (if (s-blank? venv-file-content)
      nil
      (s-trim (car (s-lines venv-file-content))))))

(defun suggest-virtual-environment-name ()
"Read the virtual environment name from the .venv file.
The .venv file is located in the project root folder."
  (let ((venv-file-path (concat (projectile-project-root) ".venv")))
    (if
      (and
        (f-exists? venv-file-path)
        (f-file? venv-file-path)
        (f-readable? venv-file-path))
      (let ((venv-name (parse-single-token venv-file-path)))
        (if (s-blank? venv-name)
          nil
          venv-name)))))

;; Repeat of what is in the setup-eshell.el file.
(defvar custom-eshell-path-env)
(setq custom-eshell-path-env eshell-path-env)

(defun activate-python-project ()
"Activate the current projectile project as a python project.
Sets the python-extra-pythonpath to the root of the project.
Activates the virtual environment."
  (if (projectile-project-p)
    (progn
      ;; Add the project root to the python-shell-extra-pythonpaths
      (setq python-shell-extra-pythonpaths original-python-extra-pythonpaths)
      (add-to-list 'python-shell-extra-pythonpaths (projectile-project-root))
      ;; If we are in a virtual environment then deactivate it.
      (if python-shell-virtualenv-root
        (pyvenv-deactivate))
      ;; Acivate the virtual environment from the .venv file if there is one.
      (let ((venv-name (suggest-virtual-environment-name)))
        (unless (s-blank? venv-name)
          (progn
            (pyvenv-workon venv-name)
            ;; Set the eshell path
            ;; https://github.com/jorgenschaefer/elpy/issues/1172
            ;; Setting eshell-path-env directly does not seem to work.
            (setq eshell-path-env (mapconcat 'identity exec-path ":"))
            ;; Instead set custom-eshell-path-env that we then use in
            ;; an eshell-mode hook to set eshell-path-env.
            (setq custom-eshell-path-env (mapconcat 'identity exec-path ":"))
            ))))))

;; On startup activate the current projectile project, if there is
;; one, as a python project.
(activate-python-project)

;; And switch python project when we switch projects in Projectile.
(add-hook 'projectile-after-switch-project-hook #'activate-python-project)

;;
;; iPython
;;

;; Use ipython for the inferior shell. The --simple-prompt argument is there
;; because Emacs does not work with the default prompt in newer versions of
;; ipython.
;; (setq python-shell-interpreter "ipython"
;;        python-shell-interpreter-args "--simple-prompt -i")

;; Actually the above might interfer when switching virtual environments.
