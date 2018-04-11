;;; setup-misc --- Miscellaneous customizations

;;; Commentary:

;; This is a dumping ground for miscellaneous functions and comments.
;; These things usually graduate from here and are moved elsewhere.

;; Print current time in minibuffer.
;; Would be good if I had a keybinding for the above.

;; (current-time-string)

;;; Code:

(defun emacs-todo ()
"Open Emacs todo org file."
  (interactive)
  (find-file "~/.emacs.d/todo.org"))

(provide 'setup-misc)
;;; setup-misc.el ends here
