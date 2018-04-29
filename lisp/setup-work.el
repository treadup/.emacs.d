;;; setup-work --- Private config for work.

;;; Commentary:

;;; Code:

(require 'f)

(defconst private-work-filename
  (concat user-emacs-directory (convert-standard-filename "private/work-setup.el")))

(when (f-exists? private-work-filename)
  (load private-work-filename))

(provide 'setup-work)
;;; setup-work ends here
