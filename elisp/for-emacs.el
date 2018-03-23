;;;
;;; Configuration of builtin emacs packages
;;;

;; Automatically revert a buffer if the file was changed by an external
;; program.
;; (global-auto-revert-mode 1)

;; Make hyper links clickable in text mode and prog mode.
(add-hook 'text-mode-hook 'goto-address-mode)
(add-hook 'prog-mode-hook 'goto-address-mode)
