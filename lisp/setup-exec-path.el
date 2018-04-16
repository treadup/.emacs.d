;;; setup-exec-path --- Setup exec path from shell

;;; Commentary:

;; On OSX if you do not start the Emacs application from a shell the PATH variable
;; will not be set correctly. To fix this you can use the exec-path-from-shell
;; package which will try to read these variables from the shell and set them
;; in Emacs.

;;; Code:

(use-package exec-path-from-shell
  :ensure t
  :config
  (setq exec-path-from-shell-shell-name "/bin/bash")
  (when (memq window-system '(mac ns x))
            (exec-path-from-shell-initialize)))

(provide 'setup-exec-path)
;;; setup-exec-path.el ends here
