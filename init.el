;;
;; Package management
;;

(require 'package)

;; We do not want to call package-initialize again after the init file has
;; been loaded.
(setq package-enable-at-startup nil)

;; Add package archives
;;(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
;;(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; Bootstrap 'use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;
;; Load packages
;;

;; The :ensure t tells use-package to load the package from the package-archives
;; if the package is not already installed.

;; Provide better default configuration for Emacs than the out of the box experience.
(use-package better-defaults
  :ensure t)

;; Magit is a git frontend.
(use-package magit
  :ensure t)

;; A dark theme for emacs.
;; https://draculatheme.com/emacs/
;; (use-package dracula-theme
;;  :ensure t)
;;
;; This has the problem that the current line highlighting disappears.

;; Lets try some of the built in themes.
;; I can't really find one that I like.
;; (load-theme 'deeper-blue)

;; Here are some other good built in themes.
;; deeper-blue
;; misterioso
;; tango-dark
;; tshd-dark
;; wheatgrass

;; A theme that I have used previously and that is the theme for the Clojure True and Brave emacs.d 
;; is the Tomorrow Night Bright.
;; Unfortunately it does not seem to be available via any package repos.
;; https://github.com/chriskempson/tomorrow-theme/blob/master/GNU%20Emacs/color-theme-tomorrow.el

(use-package base16-theme
  :ensure t
  :config
  (load-theme 'base16-solar-flare t)) ;; The t at the end load the them without giving any security warnings.

;; Some other good choices for themes
;; base16-materia
;; base16-oceanicnext
;; base16-harmonic16-dark
;; base16-solar-flare
;; base16-solarized-dark

;; The smex package provides an Ido like interface for M-x.
(use-package smex
  :ensure t
  :bind (("M-x" . smex))
  :config (smex-initialize))

;; Projectile is a project interaction library for Emacs.
;; It provides functions for project navigation.
;; https://www.emacswiki.org/emacs/Projectile
(use-package projectile
  :ensure t
  :config (projectile-global-mode)) ;; Enable projectile everywhere.

;; Rainbow colored delimiters. Extremely useful if you are programming
;; in a lisp like language.
;; https://www.emacswiki.org/emacs/RainbowDelimiters
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;;;
;;; User Interface
;;;

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

;;;
;;; Navigation
;;;

;;
;; Ido mode
;;

;; Ido mode allows you to more easily navigate choices.
(ido-mode t)

;; Use partial matching
(setq ido-enable-flex-matching t)

;; Enable ido wherever it could be useful.
(setq ido-everywhere t)

;; Change the way the list of buffers is shown.
(global-set-key (kbd "C-x C-b") 'ibuffer)

;;
;; Recent files
;;

;; Build a list of recently opened files.
;; https://www.emacswiki.org/emacs/RecentFiles
(setq recentf-save-file (concat user-emacs-directory ".recentf"))
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 40)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)


;;;
;;; Editing
;;;

;; Highlights matching parenthesis
(show-paren-mode 1)

;; Highlight current line
(global-hl-line-mode 1)

;; Interactive search key bindings. By default, C-s runs
;; isearch-forward, so this swaps the bindings.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;;;
;;; Backups and Auto Saves
;;;

;; To avoid sprinkling backups and auto save files all over the filesystem
;; we can use the following code.
;; https://snarfed.org/gnu_emacs_backup_files

;; Put autosave files (ie #foo#) and backup files (ie foo~) in ~/.emacs.d/.
(custom-set-variables
  '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
  '(backup-directory-alist '((".*" . "~/.emacs.d/backups/"))))

;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)

;; Now we have some other backup related stuff.
;; For a description of how these variables work see the following url
;; http://stackoverflow.com/questions/151945/how-do-i-control-how-emacs-makes-backup-files

;; Make backups by copying
(setq backup-by-copying t)

;; Use version numbers on backups
(setq version-control t)

;; Delete old versions. In other words do not keep excess backups.
(setq delete-old-versions t)

;; Keep 20 of the newest versions of the file.
(setq kept-new-versions 20)

;; Keep 5 of the oldest versions of the file.
(setq kept-old-versions 5) 

;; Create backups of version controlled files.
(setq vc-make-backup-files t)

;; TODO: Backup on save
;; By default Emacs does not take a backup each time you save the file.
;; It would be good if I could have Emacs do so. However it does not
;; seem super important. (Famous last words.)

;; There is a package called backup-each-save but I have not gotten it
;; to work.

;; When you visit a file, point goes to the last place where it
;; was when you previously visited the same file.
;; http://www.emacswiki.org/emacs/SavePlace
(require 'saveplace)
(setq-default save-place t)

;;;
;;; Customizations
;;;

;; Store custom elisp files in the /elisp subdirectory of the dot emacs folder. 
;; https://www.emacswiki.org/emacs/DotEmacsDotD
(add-to-list 'load-path "~/.emacs.d/elisp")

;;
;; Clojure specific setup
;;
(load "for-clojure.el")

;;
;; Shell specific setup
;;
(load "for-shell.el")
