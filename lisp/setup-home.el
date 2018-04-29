;;; setup-home --- Private config for home computers.

;;; Commentary:

;;; Code:

(require 'f)

(defconst private-home-config-filename
  (concat user-emacs-directory (convert-standard-filename "private/home-setup.el")))

(when (f-exists? private-home-config-filename)
  (load private-home-config-filename))

(provide 'setup-work)
;;; setup-home ends here
