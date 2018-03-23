;;;
;;; Customizations for whitespace
;;;

;; Always remove trailing whitespaces when saving a file.
(add-hook 'before-save-hook 'delete-trailing-whitespace)
