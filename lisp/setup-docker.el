;;;
;;; Customization for Docker
;;;

;; Docker major mode for editing Dockerfiles
(use-package dockerfile-mode
  :ensure t)

;; Tramp integration for Docker
(use-package docker-tramp
  :ensure t)

;; Manage Docker form Emacs
(use-package docker
  :ensure t)
