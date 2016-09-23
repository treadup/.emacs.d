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

;;
;; Sections
;;
;; Just add different sections here for UI, navigation, etc.
;; Later on I might want to split these out into different files
;; but probably not right now.

;;
;; User Interface
;;

;; Hide startup screen
(setq inhibit-startup-screen t)

;; Show line numbers
(global-linum-mode)

;; Show menu bar
(menu-bar-mode t)

;; Increase font size.
;; Play with the :height attribute to change size
(set-face-attribute 'default nil :height 120)

;; No cursor blinking
(blink-cursor-mode 0)

;; Title bar path
;; Full path in the title bar
(setq-default frame-title-format "%b (%f)")

;; Set the width and height of the emacs window in characters.
(setq initial-frame-alist '((top . 0) (left . 0) (width . 130) (height . 40)))

;;
;; Clipboard interaction
;;

;; In general this area is a mess. See https://www.emacswiki.org/emacs/CopyAndPaste
;; for more information.

;; Use the clipboard when killing and yanking
(setq x-select-enable-clipboard t)

;; The C-w and C-y commands should use the clipboard selection.
(setq x-select-enable-clipboard nil)

;; Save clipboard strings into kill ring before replacing them.
;; When one selects something in another program to paste it into Emacs,
;; but kills something in Emacs before actually pasting it,
;; this selection is gone unless this variable is non-nil
(setq save-interprogram-paste-before-kill t)

;;
;; Navigation
;;

;; Ido mode

