;;;
;;; Code:
;;;

;;
;; Setup GnuTLS
;;
(require 'gnutls)
(add-to-list 'gnutls-trustfiles "/etc/ssl/cert.pem")

;;
;; Package management
;;
(require 'package)

;; We do not want to call package-initialize again after the init file has
;; been loaded.
(setq package-enable-at-startup nil)

;; Add package archives
;;(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
;; (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(package-initialize)

;; To upadate the packages (or is it the index of packages?) run (package-refresh-contents)
;; You can also run M-x package-list-packages. This will also update the list of packages.

;; Bootstrap 'use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Load the diminish package early. It can then be used in the use-package forms
;; using :diminish
(use-package diminish
  :ensure t)

;; On OSX if you do not start the Emacs application from a shell the PATH variable
;; will not be set correctly. To fix this you can use the exec-path-from-shell
;; package which will try to read these variables from the shell and set them
;; in Emacs.

(use-package exec-path-from-shell
  :ensure t
  :config
  (setq exec-path-from-shell-shell-name "/bin/bash")
  (when (memq window-system '(mac ns x))
            (exec-path-from-shell-initialize)))

;;;
;;; Backups and Auto Saves
;;;

;; TODO: Split this up into two for-foo.el files namely for-backups.el and for-autosave.el

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

;; Magit is a git frontend.
(use-package magit
  :ensure t)

;;
;; Clipboard interaction
;;

;; In general this area is a mess. See https://www.emacswiki.org/emacs/CopyAndPaste
;; for more information.

;; Use the clipboard when killing and yanking
(setq select-enable-clipboard t)

;; The C-w and C-y commands should use the clipboard selection.
(setq select-enable-clipboard nil)

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

;;
;; Load path
;;

;; Store custom elisp files in the /elisp subdirectory of the dot emacs folder.
;; https://www.emacswiki.org/emacs/DotEmacsDotD
(add-to-list 'load-path "~/.emacs.d/lisp")

;;;
;;; Customizations
;;;

;;
;; Hostname functions
;;
(load "setup-hostname.el")

;;
;; Context functions
;;
(load "setup-context.el")

;;
;; EasyPG specific setup
;;
(load "setup-gpg.el")

;;
;; Auth source specific setup
;;
(load "setup-auth-source.el")

;;
;; Secrets
;;
;; (load "setup-secrets.el")

;;
;; Emacs specific setup
;;
(load "setup-emacs.el")

;;
;; Whitespace specific setup
;;
(load "setup-whitespace.el")

;;
;; Emacs server
;;
(load "setup-server.el")

;;
;; Projectile
;;
(load "setup-projectile.el")

;;
;; Rainbow delimiters
;;
(load "setup-rainbow-delimiters.el")

;;
;; User interface specific setup
;;
(load "setup-ui.el")

;;
;; Window numbering
;;
(load "setup-winum.el")

;;
;; Window Configuration
;;
(load "setup-window-configuration.el")

;;
;; Buffer move setup
;;
(load "setup-buffer-move.el")

;;
;; Theme specific setup
;;
(load "setup-theme.el")

;;
;; Ag specific setup
;;
(load "setup-ag.el")

;;
;; Company mode specific setup
;;
(load "setup-company-mode.el")

;;
;; Projectile specific setup
;;
(load "setup-projectile.el")

;;
;; Clojure specific setup
;;
(load "setup-clojure.el")

;;
;; Shell specific setup
;;
(load "setup-shell.el")

;;
;; Bash specific setup
;;
(load "setup-bash.el")

;;
;; Fish specific setup
;;
(load "setup-fish.el")

;;
;; OS X specific setup
;;

(if (string-equal system-type "darwin")
    (load "setup-osx.el"))

;;
;; Eldoc mode specific setup
;;
(load "setup-eldoc.el")

;;
;; Which key setup
;;
(load "setup-which-key.el")

;;
;; Text mode specific setup
;;
(load "setup-text.el")

;;
;; Browser specific setup
;;
(load "setup-browser.el")

;;
;; Git specific setup
;;
(load "setup-git.el")

;;
;; Tramp specific setup
;;
(load "setup-tramp.el")

;;
;; Elisp specific setup
;;
(load "setup-elisp.el")

;;
;; Ivy specific setup
;;
;; For some reason swiper does not work.
;;
;; (load "setup-ivy.el")

;;
;; Python specific setup
;;
(load "setup-python.el")

;;
;; Common Lisp specific setup
;;
(load "setup-common-lisp.el")

;;
;; Erlang specific setup
;;
(load "setup-erlang.el")

;;
;; Kill current buffer instead of prompting for buffer to kill.
;;
(load "setup-kill-buffer.el")

;;
;; Execute shell commands and place the result in the current buffer.
;;
(load "setup-shell-command.el")

;;
;; Web mode specific setup
;;
(load "setup-web-mode.el")

;;
;; Project navigation specific setup
;;
(load "setup-project-nav.el")

;;
;; Neotree specific setup
;;
;; (load "setup-neotree.el")

;;
;; Treemacs specific setup
;;
(load "setup-treemacs.el")

;;
;; Log file specific setup
;;
;;(load "setup-log.el")

;;
;; Editor Config specific setup
;; editorconfig.org
(load "setup-editor-config.el")

;;
;; Flycheck specific setup
;;
(load "setup-flycheck.el")

;;
;; SQL specific setup
;;
;; (load "setup-sql.el")

;;
;; NGINX specific setup
;;
(load "setup-nginx.el")

;;
;; Docker specific setup
;;
(load "setup-docker.el")

;;
;; Ascii Doc specific setup
;;
(load "setup-asciidoc.el")

;;
;; Markdown specific setup
;;
(load "setup-markdown.el")

;;
;; Scheme specific setup
;;
(load "setup-scheme.el")

;;
;; Go lang specific setup
;;
(load "setup-go.el")

;;
;; YAML specific setup
;;
(load "setup-yaml.el")

;;
;; JSON specific setup
;;
(load "setup-json.el")

;;
;; JavaScript specific setup
;;
(load "setup-javascript.el")

;;
;; Eyebrowse specific setup
;;
(load "setup-eyebrowse.el")

;;
;; Todo specific setup
;;
(load "setup-todo.el")

;;
;; Multi term setup
;;
(load "setup-multi-term.el")

;;
;; Eshell setup
;;
(load "setup-eshell.el")

;;
;; Spotify setup
;;
(load "setup-spotify.el")

;;
;; html to markdown setup
;;
(load "setup-html-to-markdown.el")

;;
;; Miscellaneous customizations
;;
(load "setup-misc.el")

;;
;; Libraries for Elisp
;;
(load "setup-libraries.el")

;;
;; Guru mode
;;
(load "setup-guru.el")

;;
;; Pass specific setup
;;
;; (load "setup-pass.el")

;;
;; Wanderlust email client
;;
;; Right now I cannot load the wanderlust email client from MELPA.
;; I'm going to leave this in there and see if it works later on.
;; (load "setup-wanderlust.el")

(provide 'init)
;;; init.el ends here
