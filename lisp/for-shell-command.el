;;;
;;; Customizations for shell command execution
;;;
(defun shell-command-to-buffer (cmd)
  (interactive)
  (insert (shell-command-to-string cmd)))

