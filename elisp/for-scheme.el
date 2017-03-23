;;;
;;; Customization for Scheme
;;;

;; Tell geiser to only use the guile implementation.
;; This allows us to skip having to enter which scheme implementation
;; to use each time we execute geiser.
(setq geiser-active-implementations '(guile)) ;; '(chicken guile)

(use-package geiser
  :ensure t)

