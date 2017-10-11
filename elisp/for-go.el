;;;
;;; Customization for the Go programming language.
;;;

(use-package go-mode
  :ensure t)

(add-hook 'go-mode-hook
          (lambda ()
            ;; (add-hook 'before-save-hook 'gofmt-before-save)
            (setq indent-tabs-mode 1)))
