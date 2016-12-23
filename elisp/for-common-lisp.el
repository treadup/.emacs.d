;;;
;;; Customizations for Common Lisp
;;;

(use-package slime
  :ensure t
  :config
  (progn
    (setq inferior-lisp-program "/usr/local/bin/sbcl")
    (setq slime-contribs '(slime-fancy))))
