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

;;;
;;; Backups and Auto Saves
;;;

;;
;; Issue: This section uses the custom-set-variables function. This function has to be
;; called before themes and other things are loaded. There is some kind of issue here
;; with the order of the custom-set-variables function being called and functions
;; that use the customize system.
;;

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

;;
;; Customize system
;;

;; http://irreal.org/blog/?p=3765
;; http://emacs.stackexchange.com/questions/102/advantages-of-setting-variables-with-setq-instead-of-custom-el
;; http://stackoverflow.com/questions/5052088/what-is-custom-set-variables-and-faces-in-my-emacs
;; https://www.reddit.com/r/emacs/comments/2lif7v/how_to_transform_your_customsetvariables_in/
;; http://stackoverflow.com/questions/8545756/how-to-treat-my-custom-emacs-theme-as-a-safe-theme
;;
;; Safe code. The error message about safe code has something to do with custom-set-variables not having been
;; called before the theme is loaded.
;; I do not want generated code in my init.el file.
;; Make the customize system write to a temp file that is deleted.
(setq custom-file (make-temp-file "emacs-custom"))

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

(defun hostname ()
  (car (split-string system-name "[.]")))

;;
;; User interface specific setup
;;
(load "for-ui.el")

;;
;; Theme specific setup
;;
(load "for-theme.el")

;;
;; Auto complete specific setup
;;
(load "for-auto-complete.el")

;;
;; Clojure specific setup
;;
(load "for-clojure.el")

;;
;; Shell specific setup
;;
(load "for-shell.el")

;;
;; PHP specific setup
;;
(load "for-php.el")

;;
;; OS X specific setup
;;

(if (string-equal system-type "darwin")
    (load "for-osx.el"))

;;
;; Block spacebar key
;;
(load "block-spacebar.el")

;;
;; Text mode specific setup
;;
(load "for-text.el")

;;
;; Browser specific setup
;;
(load "for-browser.el")

;;
;; Magit specific setup
;;
(load "for-magit.el")

;;
;; Tramp specific setup
;;
(load "for-tramp.el")

;;
;; elisp specific setup 
;;
(load "for-elisp.el")

;;
;; Ivy specific setup
;;
(load "for-ivy.el")

;;
;; Python specific setup
;;
(load "for-python.el")

;;
;; Common Lisp specific setup
;;
(load "for-common-lisp.el")

;;
;; Kill current buffer instead of prompting for buffer to kill.
;;
(load "for-kill-buffer.el")

;;
;; Execute shell commands and place the result in the current buffer.
;;
(load "for-shell-command.el")

;;
;; Web mode specific setup
;;
(load "for-web-mode.el")

;;
;; Per host specific setup
;;
(cond
 ((string= "hickory" (hostname)) (load "hickory-configuration.el"))
 (t (load "default-configuration.el")))
