;;; setup-emacs --- Configuration of builtin emacs packages.

;;; Commentary:

;;; Code:
;; Automatically revert a buffer if the file was changed by an external
;; program.
;; (global-auto-revert-mode 1)

;; Make hyper links clickable in text mode and prog mode.
(add-hook 'text-mode-hook 'goto-address-mode)
(add-hook 'prog-mode-hook 'goto-address-mode)

;; Some functions in Emacs are disabled by default.
;; The following will enable individual functions.

;; Enable the erase-buffer function.
(put 'erase-buffer 'disabled nil)

(provide 'setup-emacs)
;;; setup-emacs.el ends here
