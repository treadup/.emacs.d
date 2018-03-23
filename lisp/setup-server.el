;;
;; Customizations for the Emacs server
;;

;; Only run the server as a non root user.
(unless (string-equal "root" (user-login-name))
  (require 'server)
  (unless (server-running-p) (server-start)))
