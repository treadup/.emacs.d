;;;
;;; Customization for Docker
;;;

;; Major mode for editing Dockerfiles
(use-package dockerfile-mode
  :ensure t)

;; Major mode for editing docker-compose files
(use-package docker-compose-mode
  :ensure t)

;; Tramp integration for Docker
(use-package docker-tramp
  :ensure t)

;; Manage Docker form Emacs
(use-package docker
  :ensure t)
