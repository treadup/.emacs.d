;;; setup-theme --- Setup the theme and modeline

;;; Commentary:

;;; Code:

;; There is an issue with the spacemacs theme not being installable using use-package.
;; This has to to with the naming of the package and the names of the files in the package.
;; Instead we can use plain old package.el to install the package.
(unless (package-installed-p 'spacemacs-theme)
  (package-refresh-contents)
  (package-install 'spacemacs-theme))

(load-theme 'spacemacs-dark t)
;; (load-theme 'spacemacs-light t)

(defun is-macos ()
  "Return true if we are running on macOS."
  (string-equal system-type "darwin"))

;; Use the modeline from spacemacs
(use-package spaceline-config
  :ensure spaceline
  :config

  ;; For macOS fix the colors of the separator characters in the powerline.
  (when (is-macos)
    (setq powerline-image-apple-rgb t))

  (spaceline-spacemacs-theme))

;; Use the wave separator character in the modeline instead of the arrow.
;; To get this to work you might have to execute (spaceline-compile)
(setq powerline-default-separator 'wave)
(spaceline-compile)

(provide 'setup-theme)
;;; setup-theme.el ends here
