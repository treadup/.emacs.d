;;; vpy --- Python virtual environment switcher inspired by virtualfish.

;;; Commentary:

;; A good name for this package might be viper or vipyr.

;; Perhaps virtual-viper with the short name vv.
;; so you would do vv activate, etc.

;;; Code:
(require 'eshell)

(defconst vpy--help-message "Usage: vpy <command> [<args>]
The vpy command is used to activate and deactivate Python virtual
environments in eshell.

The following commands are available.
    help <command>                 Shows the help message for the command.
    activate <virtual env name>    Activates the named virtual environment.
    deactivate                     Deactivates the current virtual environment.
")

(defconst vpy--help-help-message "Usage: vpy help <command> ")

(defconst vpy--activate-help-message "Usage: vpy activate <virtual env name>")

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
        ("activate"   (eshell/echo vpy--activate-help-message))
        ("deactivate" (eshell/echo vpy--deactivate-help-message))
        (_            (eshell/echo (concat "Unknown command " cmd)))))
    (eshell/echo vpy--help-help-message)))

(defconst vpy--original-eshell-path-env eshell-path-env)
(defvar-local vpy--current-eshell-path-env vpy--original-eshell-path-env)
(defvar-local vpy--current-venv-name nil)
(defvar-local vpy-venv-name nil)

(defun vpy--virtual-environment-bin-directory (venv-name)
"Find the path to the virutal environment bin directory.
VENV-NAME is the name of the virtual environment."
  (concat (find-python-virtualenv-path venv-name) "/bin"))

(defun vpy-activate (&optional venv-name &rest args)
"Activate the VENV-NAME virtual environment.
ARGS should be nil."
  (if (or (not (null args)) (null venv-name))
    (eshell/echo vpy--activate-help-message)
    (progn
      (setq-local vpy--current-venv-name venv-name)
      (setq-local vpy--current-eshell-path-env
        (concat (vpy--virtual-environment-bin-directory venv-name) ":"
          vpy--original-eshell-path-env))
      (setq eshell-path-env vpy--current-eshell-path-env)
      nil)))

(defun vpy-deactivate (&rest args)
"Deactivate the currently active virtual environment.
ARGS should be nil."
  (if (not (null args))
    (eshell/echo vpy--deactivate-help-message)
    (progn
      (setq-local vpy--current-venv-name nil)
      (setq-local vpy--current-eshell-path-env vpy--original-eshell-path-env)
      (setq eshell-path-env vpy--current-eshell-path-env)
      nil)))

(defun eshell/vpy (&optional cmd &rest args)
"The vpy function is used to manage Python virtual environments.
Executes the given CMD.  ARGS are dependent on the command you execute."
  (if (null cmd)
    (vpy-help)
    (pcase cmd
      ("help" (apply 'vpy-help args))
      ("activate" (apply 'vpy-activate args))
      ("deactivate" (apply 'vpy-deactivate args))
      (_ (eshell/echo (concat "Unknown command " cmd))))))

(provide 'vpy)
;;; vpy.el ends here
