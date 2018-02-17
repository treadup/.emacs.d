;;;
;;; Customizations for hostname
;;;

(defun hostname ()
  "Get the hostname of the computer Emacs is running on."
  (car (split-string (system-name) "[.]")))

(defun is-host-p (name)
  "Determine if the hostname is equal to the given NAME."
  (equal name (hostname)))

(provide 'for-hostname)
;;; for-hostname.el ends here
