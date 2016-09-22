;;
;; Package management
;;

(require 'package)

;; We do not want to call package-initialize again after the init file has
;; been loaded.
(setq package-enable-at-startup nil)

;; Add package archives
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(defvar my-packages '(better-defaults
                      projectile
                      clojure-mode
                      cider))

;; Bootstrap 'use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;
;; Load packages
;;

;; The :ensure t tells use-package to load the package from the package-archives
;; if the package is not already installed.

(use-package better-defaults
  :ensure t)

(use-package magit
  :ensure t)

;; A dark theme for emacs.
;; https://draculatheme.com/emacs/
(use-package dracula-theme
  :ensure t)

;; Hide startup screen
(setq inhibit-startup-screen t)

