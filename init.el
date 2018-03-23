;;;
;;; Code:
;;;

;; There is a package called auto-compile that looks interesting.
;; https://github.com/emacscollective/auto-compile
(setq load-prefer-newer t)

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

(message "Checking ido mode after initialize")
(debug-has-ido)

(package-initialize)

(message "Checking ido mode after initialize")
(debug-has-ido)

;; To upadate the packages (or is it the index of packages?) run (package-refresh-contents)
;; You can also run M-x package-list-packages. This will also update the list of packages.

;; If you remove a use-package statement for a package that package will still stay installed
;; if it has already been installed. Removing a use-package statement will never uninstall
;; a package. You have to do this manually. One simple way of doing this is to remove the elpa
;; folder in the emacs user directory.

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

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;;
;; Load path
;;

;; Store custom elisp files in the /elisp subdirectory of the dot emacs folder.
;; https://www.emacswiki.org/emacs/DotEmacsDotD
(add-to-list 'load-path "~/.emacs.d/lisp")


(defun setup-custom (filename)
  "Load custom settings from the file with name FILENAME."
;;  (message (concat "Loading " filename))
  (load filename))

;;;
;;; Customizations
;;;

;;
;; Hostname functions
;;
(setup-custom "setup-hostname.el")

;;
;; Context functions
;;
(setup-custom "setup-context.el")

;;
;; EasyPG specific setup
;;
(setup-custom "setup-gpg.el")

;;
;; Auth source specific setup
;;
(setup-custom "setup-auth-source.el")

;;
;; Secrets
;;
;; (setup-custom "setup-secrets.el")

;;
;; Emacs specific setup
;;
(setup-custom "setup-emacs.el")

;;
;; Whitespace specific setup
;;
(setup-custom "setup-whitespace.el")

;;
;; Emacs server
;;
(setup-custom "setup-server.el")

;;
;; Projectile
;;
(setup-custom "setup-projectile.el")

;;
;; Rainbow delimiters
;;
(setup-custom "setup-rainbow-delimiters.el")

;;
;; User interface specific setup
;;
(setup-custom "setup-ui.el")

;;
;; Window numbering
;;
(setup-custom "setup-winum.el")

;;
;; Window Configuration
;;
(setup-custom "setup-window-configuration.el")

;;
;; Buffer move setup
;;
(setup-custom "setup-buffer-move.el")

;;
;; Theme specific setup
;;
(setup-custom "setup-theme.el")

;;
;; Ag specific setup
;;
(setup-custom "setup-ag.el")

;;
;; Company mode specific setup
;;
(setup-custom "setup-company-mode.el")

;;
;; Projectile specific setup
;;
(setup-custom "setup-projectile.el")

;;
;; Clojure specific setup
;;
(setup-custom "setup-clojure.el")

;;
;; Shell specific setup
;;
(setup-custom "setup-shell.el")

;;
;; Bash specific setup
;;
(setup-custom "setup-bash.el")

;;
;; Fish specific setup
;;
(setup-custom "setup-fish.el")

;;
;; OS X specific setup
;;

(if (string-equal system-type "darwin")
    (setup-custom "setup-osx.el"))

;;
;; Eldoc mode specific setup
;;
(setup-custom "setup-eldoc.el")

;;
;; Which key setup
;;
(setup-custom "setup-which-key.el")

;;
;; Text mode specific setup
;;
(setup-custom "setup-text.el")

;;
;; Browser specific setup
;;
(setup-custom "setup-browser.el")

;;
;; Git specific setup
;;
(setup-custom "setup-git.el")

;;
;; Tramp specific setup
;;
(setup-custom "setup-tramp.el")

;;
;; Elisp specific setup
;;
(setup-custom "setup-elisp.el")

;;
;; Ivy specific setup
;;
;; For some reason swiper does not work.
;;
;; (setup-custom "setup-ivy.el")

;;
;; Python specific setup
;;
(setup-custom "setup-python.el")

;;
;; Common Lisp specific setup
;;
(setup-custom "setup-common-lisp.el")

;;
;; Erlang specific setup
;;
(setup-custom "setup-erlang.el")

;;
;; Kill current buffer instead of prompting for buffer to kill.
;;
(setup-custom "setup-kill-buffer.el")

;;
;; Execute shell commands and place the result in the current buffer.
;;
(setup-custom "setup-shell-command.el")

;;
;; Web mode specific setup
;;
(setup-custom "setup-web-mode.el")

;;
;; Project navigation specific setup
;;
(setup-custom "setup-project-nav.el")

;;
;; Neotree specific setup
;;
;; (setup-custom "setup-neotree.el")

;;
;; Treemacs specific setup
;;
(setup-custom "setup-treemacs.el")

;;
;; Log file specific setup
;;
;;(setup-custom "setup-log.el")

;;
;; Editor Config specific setup
;; editorconfig.org
(setup-custom "setup-editor-config.el")

;;
;; Flycheck specific setup
;;
(setup-custom "setup-flycheck.el")

;;
;; SQL specific setup
;;
;; (setup-custom "setup-sql.el")

;;
;; NGINX specific setup
;;
(setup-custom "setup-nginx.el")

;;
;; Docker specific setup
;;
(setup-custom "setup-docker.el")

;;
;; Ascii Doc specific setup
;;
(setup-custom "setup-asciidoc.el")

;;
;; Markdown specific setup
;;
(setup-custom "setup-markdown.el")

;;
;; Scheme specific setup
;;
(setup-custom "setup-scheme.el")

;;
;; Go lang specific setup
;;
(setup-custom "setup-go.el")

;;
;; YAML specific setup
;;
(setup-custom "setup-yaml.el")

;;
;; JSON specific setup
;;
(setup-custom "setup-json.el")

;;
;; JavaScript specific setup
;;
(setup-custom "setup-javascript.el")

;;
;; Eyebrowse specific setup
;;
(setup-custom "setup-eyebrowse.el")

;;
;; Todo specific setup
;;
(setup-custom "setup-todo.el")

;;
;; Multi term setup
;;
(setup-custom "setup-multi-term.el")

;;
;; Eshell setup
;;
(setup-custom "setup-eshell.el")

;;
;; Spotify setup
;;
(setup-custom "setup-spotify.el")

;;
;; html to markdown setup
;;
(setup-custom "setup-html-to-markdown.el")

;;
;; Miscellaneous customizations
;;
(setup-custom "setup-misc.el")

;;
;; Libraries for Elisp
;;
(setup-custom "setup-libraries.el")

;;
;; Guru mode
;;
(setup-custom "setup-guru.el")

;;
;; Pass specific setup
;;
;; (setup-custom "setup-pass.el")

;;
;; Wanderlust email client
;;
;; Right now I cannot load the wanderlust email client from MELPA.
;; I'm going to leave this in there and see if it works later on.
;; (setup-custom "setup-wanderlust.el")

(provide 'init)
;;; init.el ends here
