;;; vpy --- Python virtual environment switcher inspired by virtualfish.

;;; Commentary:

;; A good name for this package might be viper or vipyr.

;; Perhaps virtual-viper with the short name vv.
;; so you would do vv activate, etc.

;;; Code:
(require 'eshell)
(require 's)
(require 'f)

(defconst vpy--help-message "Usage: vpy <command> [<args>]
The vpy command is used to activate and deactivate Python virtual
environments in eshell.

The following commands are available.
    help <command>                 Shows the help message for the command.
    create <virtual env name>      Creates new named virtual environment
    activate <virtual env name>    Activates the named virtual environment.
    connect                        Connects the active virtual environment with the current directory
    deactivate                     Deactivates the current virtual environment.
")

(defconst vpy--help-help-message "Usage: vpy help <command> ")
(defconst vpy--create-help-message "Usage: vpy create <virtual env name>")
(defconst vpy--activate-help-message "Usage: vpy activate <virtual env name>")
(defconst vpy--connect-help-message "Usage: vpy connect <virtual env name>")
(defconst vpy--deactivate-help-message "Usage: vpy deactivate")


(defun vpy-help (&optional cmd &rest args)
"Show the help text for the command.
If no CMD is given show the global help text.
If a CMD is given show the help text for the command.
ARGS should be nil."
  (if (null args)
    (if (null cmd)
      (eshell/echo vpy--help-message)
      (pcase cmd
        ("help"       (eshell/echo vpy--help-help-message))
        ("create"     (eshell/echo vpy--create-help-message))
        ("activate"   (eshell/echo vpy--activate-help-message))
        ("connect"    (eshell/echo vpy--connect-help-message))
        ("deactivate" (eshell/echo vpy--deactivate-help-message))
        (_            (eshell/echo (concat "Unknown command " cmd)))))
    (eshell/echo vpy--help-help-message)))

(defconst vpy--original-eshell-path-env eshell-path-env)
(defvar-local vpy--current-eshell-path-env vpy--original-eshell-path-env)
(defvar-local vpy--current-venv-name nil)
(defvar-local vpy-venv-name nil)

;; Folder containing the Python virtual environments.
(defvar vpy-virtualenv-workon-dir (expand-file-name "~/.virtualenvs/"))

(defun vpy--virtual-environment-directory (venv-name)
  "Find the path to the Python virtual environment with the given VENV-NAME."
  (concat (file-name-as-directory vpy-virtualenv-workon-dir) venv-name))

(defun vpy--virtual-environment-bin-directory (venv-name)
"Find the path to the virutal environment bin directory.
VENV-NAME is the name of the virtual environment."
  (concat (vpy--virtual-environment-directory venv-name) "/bin"))

(defun vpy--deactivate-virtual-environment ()
"Deactivate the current active Python virtual environment."
  (setq-local vpy--current-venv-name nil)
  (setq-local vpy--current-eshell-path-env vpy--original-eshell-path-env)
  (setq eshell-path-env vpy--current-eshell-path-env))

(defun vpy--activate-virtual-environment (venv-name)
  "Activate the VENV-NAME Python virtual environment."
  (if (s-blank? venv-name)
    (vpy--deactivate-virtual-environment)
    (progn
      (setq-local vpy--current-venv-name venv-name)
      (setq-local vpy--current-eshell-path-env
        (concat (vpy--virtual-environment-bin-directory venv-name) ":"
          vpy--original-eshell-path-env))
      (setq eshell-path-env vpy--current-eshell-path-env))))

;;
;; Automatic virtualenv loading
;;

(defun vpy--parse-dot-venv-file (filename)
"Parse a .venv file with the given FILENAME and return the name of the virtual environment."
  (let ((venv-file-content (f-read-text filename)))
    (if (s-blank? venv-file-content)
      nil
      (s-trim (car (s-lines venv-file-content))))))

(defun vpy--find-dot-venv-filename (dir)
  "Find the name of the .venv file associated with the given directory DIR."
  (let ((dot-venv-directory (locate-dominating-file default-directory ".venv")))
    (if dot-venv-directory
      (concat dot-venv-directory ".venv")
      nil)))

(defun vpy--find-automatic-venv-name (dir)
"Find the name of the virtual environment from the .venv file.
Either in the given directory DIR in one of the ancestors."
  (let ((venv-filename (vpy--find-dot-venv-filename dir)))
    (if venv-filename
      (vpy--parse-dot-venv-file venv-filename)
      nil)))

;; The vpy-auto-activate-venv specifies if the virtual environment should be
;; automatically activated/deactivated when navigating to a new directory.
(defvar vpy-auto-activate-venv t)

(defun vpy--eshell-before-prompt-hook ()
"Automatically activate or deactivate a Python virtual environment.
This function should be added to the eshell-before-prompt hook. This
hook is called by Eshell before displaying the prompt."
  (when vpy-auto-activate-venv
    (let ((new-venv-name (vpy--find-automatic-venv-name default-directory)))
      (unless (equal new-venv-name vpy--current-venv-name)
        (vpy--activate-virtual-environment new-venv-name)))))

(add-hook 'eshell-before-prompt-hook 'vpy--eshell-before-prompt-hook)

;;
;; Eshell commands
;;

(defun vpy-create (&optional venv-name &rest args)
 "Create named virutal environment with name VENV-NAME.
ARGS should be nil."
  (if (or (not (null args)) (null venv-name))
    (eshell/echo vpy--create-help-message)
    (progn
      (eshell/echo "Creating Python virtual environment. This might take a while...")
      (eshell/echo
        (shell-command-to-string
          (concat "/usr/bin/env python3 -m venv " vpy-virtualenv-workon-dir venv-name)))
      nil)))

(defun vpy-activate (&optional venv-name &rest args)
"Activate the VENV-NAME virtual environment.
ARGS should be nil."
  (if (or (not (null args)) (null venv-name))
    (eshell/echo vpy--activate-help-message)
    (progn
      (vpy--activate-virtual-environment venv-name)
      nil)))

(defun vpy-deactivate (&rest args)
"Deactivate the currently active virtual environment.
ARGS should be nil."
  (if (not (null args))
    (eshell/echo vpy--deactivate-help-message)
    (progn
      (vpy--deactivate-virtual-environment)
      nil)))

(defun vpy-connect (&optional venv-name &rest args)
"Connect the virtual environment with name VENV-NAME to the current directory.
ARGS should be nil."
  (if (or (not (null args)) (null venv-name))
    (eshell/echo vpy--connect-help-message)
    (let ((dot-venv-filename (concat (file-name-as-directory (eshell/pwd)) ".venv")))
      (f-write-text venv-name 'utf-8 dot-venv-filename))))

(defun eshell/vpy (&optional cmd &rest args)
"The vpy function is used to manage Python virtual environments.
Executes the given CMD.  ARGS are dependent on the command you execute."
  (if (null cmd)
    (vpy-help)
    (pcase cmd
      ("help" (apply 'vpy-help args))
      ("create" (apply 'vpy-create args))
      ("activate" (apply 'vpy-activate args))
      ("connect" (apply 'vpy-connect args))
      ("deactivate" (apply 'vpy-deactivate args))
      (_ (eshell/echo (concat "Unknown command " cmd))))))

(defun vpy-current-venv ()
"Return the name of the currently active Python virtual environment.
Return nil if no virtual environment is active."
  vpy--current-venv-name)

(provide 'vpy)
;;; vpy.el ends here
