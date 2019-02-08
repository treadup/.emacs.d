;;; setup-flycheck --- Customization for flycheck

;;; Commentary:
;; Flycheck provides modern on the fly syntax checking.
;; http://www.flycheck.org/en/latest/

;; For Python things are set up in the following manner.
;; Both pylint and flake8 are installed in the global python3 environment.
;; Flycheck is configured to use python3 as the executable for the following
;; checkers.
;;
;; 1. python-pycompile
;; 2. python-pylint
;; 3. python-flake8
;;
;; Which checker that is run is determined by the existance of a pylint or flake8
;; configuration file.
;;
;; No configuration files means run the python-pycompile checker.
;;
;; The existance of a pylint configuration file means run the python-pylint checker.
;;
;; The existance of a flake8 configuration file means run the python-flake8 checker.

;;; Code:

(use-package flycheck
  :ensure t
  :diminish "FC"
  :config

  ;; Use the global python3 executable.
  (setq flycheck-python-pycompile-executable "python3")

  ;; Use the pylint module from the global python3 environment.
  ;; In other words you have to have done a pip3 install pylint
  ;; for the following to work.
  (setq flycheck-python-pylint-executable "python3")

  ;; Use the flake8 module from the global python3 environment.
  ;; In other words you have to have done a pip3 install flake8
  ;; for the following to work.
  (setq flycheck-python-flake8-executable "python3")
  (global-flycheck-mode))

(provide 'setup-flycheck)
;;; setup-flycheck ends here
