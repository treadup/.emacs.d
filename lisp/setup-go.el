;;;
;;; Customization for the Go programming language.
;;;

(use-package go-mode
  :ensure t)

;; Use spaces in Emacs buffers for Go code. Convert
;; tabs to spaces when loading the file. Convert spaces
;; to tabs when saving the file.
(add-hook 'go-mode-hook
  (lambda ()
    (setq tab-width 4)
    (setq standard-indent 4)
    (setq indent-tabs-mode nil)
    ;; When a file is loaded untabify it.
    (untabify-buffer)
    ;; Gofmt the buffer before saving it. This will add back the tabs.
    (add-hook 'before-save-hook 'gofmt-before-save)
    ;; Remove the tabs again after the buffer has been saved.
    (add-hook 'after-save-hook 'untabify-buffer)))

(provide 'setup-go)
;;; setup-go.el ends here
