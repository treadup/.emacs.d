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

;; Right now there is an issue with anaconda-mode that causes freezes when
;; typing. See if disabling anaconda-mode fixes the problem.

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

;; TODO: Have a flag where you can enable or disable the old Eshell support
;; for python virtual envs.

;; Folder containing the Python virtual environments.
(defconst python-virtualenv-workon-dir (expand-file-name "~/.local/share/virtualenvs/"))

(defun parse-dot-venv-file (filename)
"Parse a .venv file with the given FILENAME  and return the name of the virtual environment."
  (let ((venv-file-content (f-read-text filename)))
    (if (s-blank? venv-file-content)
      nil
      (s-trim (car (s-lines venv-file-content))))))

(defun find-python-virtualenv-path (venv-name)
  "Find the path to the Python virtual environment with the given VENV-NAME."
  (concat (file-name-as-directory python-virtualenv-workon-dir) venv-name))

(defun find-dot-venv-filename (dir)
  "Find the name of the .venv file associated with the given directory DIR."
  (let ((dot-venv-directory (locate-dominating-file default-directory ".venv")))
    (if dot-venv-directory
      (concat dot-venv-directory ".venv")
      nil)))

(defun find-automatic-venv-name (dir)
  "Find the name of the virtual environment from a .venv file.
Either in the given directory DIR in one of the ancestors."
  (let ((venv-filename (find-dot-venv-filename dir)))
    (if venv-filename
      (parse-dot-venv-file venv-filename)
      nil)))

;;
;; Projectile switch project hook
;;

;; There are two things that you need to configure for Python in Emacs.
;; The first is adding the root folder of the project as an extra python path.
;; The second is activating the virtual environment.

(defconst original-python-extra-pythonpaths python-shell-extra-pythonpaths)

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
      (let ((venv-name (find-automatic-venv-name default-directory)))
        (unless (s-blank? venv-name)
          (progn
            (pyvenv-workon venv-name)
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
