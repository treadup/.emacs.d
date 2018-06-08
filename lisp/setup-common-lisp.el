;;; setup-common-lisp --- Customizations for Common Lisp

;;; Commentary:

;;; Code:

;; (use-package slime
;;   :ensure t
;;   :config
;;   (progn
;;     (setq inferior-lisp-program "/usr/local/bin/sbcl")
;;     (setq slime-contribs '(slime-fancy))))

(let ((slime-helper-filename (expand-file-name "~/.quicklisp/slime-helper.el")))
  (when (file-exists-p slime-helper-filename)
    (progn
      (load slime-helper-filename)
      (setq inferior-lisp-program "sbcl"))))

(provide 'setup-common-lisp)
;;; setup-common-lisp ends here
