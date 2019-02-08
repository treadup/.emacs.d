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

;; Emacs integration for the black code formatter for Python.
;; The black executable needs to be installed for this to work.
;; https://melpa.org/#/blacken
;; https://github.com/proofit404/blacken/
(use-package blacken
  :ensure t)

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
;; (use-package pyvenv
;;  :ensure t)

;; Folder containing the Python virtual environments.
(defconst python-virtualenv-workon-dir (expand-file-name "~/.virtualenvs/"))

(defun parse-dot-venv-file (filename)
"Parse a .venv file with the given FILENAME  and return the name of the virtual environment."
  (let ((venv-file-content (f-read-text filename)))
    (if (s-blank? venv-file-content)
      nil
      (s-trim (car (s-lines venv-file-content))))))

(defun find-python-virtualenv-path (venv-name)
  "Find the path to the Python virtual environment with the given VENV-NAME."
  (concat (file-name-as-directory python-virtualenv-workon-dir) venv-name))

(defun find-dot-venv-filename ()
  "Find the name of the .venv file associated with the current file."
  (let ((dot-venv-directory (locate-dominating-file default-directory ".venv")))
    (if dot-venv-directory
      (concat dot-venv-directory ".venv")
      nil)))

(defun find-dot-venv-directory ()
  "Find the name of the directory containing the .venv file associated with the current file."
  (locate-dominating-file default-directory ".venv"))

(defun find-automatic-venv-name ()
  "Find the name of the virtual environment associated with the current file."
  (let ((venv-filename (find-dot-venv-filename)))
    (if venv-filename
      (parse-dot-venv-file venv-filename)
      nil)))

;; There are two things that you need to configure for Python in Emacs.
;; The first is adding the root folder of the project as an extra python path.
;; The second is activating the virtual environment.

(defun venv-find-executable (executable)
  "Find an EXECUTABLE. Search the current path.
If there is a virtual environment then search the bin
folder of the virtual environment as well."
  (let ((exec-path (python-shell-calculate-exec-path)))
    (executable-find executable)))

(defun venv-only-find-executable (executable)
  "Find an EXECUTABLE searching only the bin folder in the current virtual environment."
  (if python-shell-virtualenv-root
    (let ((exec-path (list (concat python-shell-virtualenv-root "bin"))))
      (executable-find executable))))

(defconst original-python-shell-extra-pythonpaths python-shell-extra-pythonpaths)

(defun venv-activate (dir)
  "Activate the Python virtual environment located at DIR."
  (interactive "DVirtualenv directory: ")
  (pythonic-activate dir))

(defun venv-deactivate ()
  "Deactivate the currently active Python virtual environment."
  (interactive)
  (progn
    (pythonic-deactivate)
    (setq python-shell-extra-pythonpaths original-python-shell-extra-pythonpaths)))

(defun venv-names ()
  "Return the list of the known virtual environment names."
  ;; In the future we might just want this to be a hard coded
  ;; list instead. Currently the returned list contains a bunch
  ;; of garbage virtual environment names.
  (mapcar 'f-filename (f-directories python-virtualenv-workon-dir)))

(defun venv-workon (venv-name)
  "Activate the Python virtual environment with the name VENV-NAME.
The name of the virtual environment is entered interactively."
  (interactive
    (list (completing-read "Virtual env name: " (venv-names) nil t)))
  (venv-activate (concat python-virtualenv-workon-dir venv-name)))

(defun venv-auto ()
  "Activate the Python virtual environment associated with the current file.
If the current buffer does not have an associated file then do nothing."
  (interactive)
  (let ((venv-name (find-automatic-venv-name)))
    (if venv-name
      (let ((venv-directory (find-dot-venv-directory)))
        (when venv-directory
          (progn
            (setq python-shell-extra-pythonpaths
              (cons venv-directory original-python-shell-extra-pythonpaths))
            (venv-workon venv-name))))
      (venv-deactivate))))

(define-key python-mode-map (kbd "C-c C-e") 'venv-auto)

;;
;; iPython
;;

;; Use ipython for the inferior shell. The --simple-prompt argument is there
;; because Emacs does not work with the default prompt in newer versions of
;; ipython.
;; (setq python-shell-interpreter "ipython"
;;        python-shell-interpreter-args "--simple-prompt -i")

;; Actually the above might interfer when switching virtual environments.

(provide 'setup-python)
;;; setup-python.el ends here
