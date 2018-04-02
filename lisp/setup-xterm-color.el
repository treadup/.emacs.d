;;; setup-xterm-color --- Configure shell and eshell to use 256 colors

;;; Commentary:
;; Provide support for 256 colors in shell and eshell.  Can also be used
;; to provide 256 colors in compilation buffers.
;; https://github.com/atomontage/xterm-color

;;; Code:
(require 'eshell)

(use-package xterm-color
  :ensure t
  :config

  ;; Eshell
  (add-hook 'eshell-before-prompt-hook
    (lambda ()
      (setq xterm-color-preserve-properties t)))

  (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
  (setq eshell-output-filter-functions
    (remove 'eshell-handle-ansi-color eshell-output-filter-functions))

  (setenv "TERM" "xterm-256-color"))

(provide 'setup-xterm-color)
;;; setup-xterm-color ends here
