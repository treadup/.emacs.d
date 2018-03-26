;;;
;;; Code:
;;;

;; Byte compile all files in the .emacs.d/lisp folder. Force byte compilation
;; even if there is no existing .elc file for the .el file.
(byte-recompile-directory "~/.emacs.d/lisp" 0)

;; To revaluate elisp code you can use one of the following options.
;; C-M-x will call eval-defun which evaluates the top level form
;; containing point.
;; C-x C-e will call eval-last-sexp which evaluates the sexp before
;; point and then prints the value in the echo area.
;; To evaluate the entire buffer you can call M-x eval-buffer
;; To evaluate the region you can call M-x eval-region

;; If the .el file is newer than the .elc file then load the .el file.
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

(package-initialize)

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

;; Required when bytecompiling to avoid personal-keybindings is void error that seems to be
;; caused by the use-package macro.
(require 'bind-key)

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
(setup-custom "setup-hostname")

;;
;; Context functions
;;
(setup-custom "setup-context")

;;
;; EasyPG specific setup
;;
(setup-custom "setup-gpg")

;;
;; Auth source specific setup
;;
(setup-custom "setup-auth-source")

;;
;; Emacs specific setup
;;
(setup-custom "setup-emacs")

;;
;; Whitespace specific setup
;;
(setup-custom "setup-whitespace")

;;
;; Emacs server
;;
(setup-custom "setup-server")

;;
;; Projectile
;;
(setup-custom "setup-projectile")

;;
;; Rainbow delimiters
;;
(setup-custom "setup-rainbow-delimiters")

;;
;; User interface specific setup
;;
(setup-custom "setup-ui")

;;
;; Window numbering
;;
(setup-custom "setup-winum")

;;
;; Window Configuration
;;
(setup-custom "setup-window-configuration")

;;
;; Buffer move setup
;;
(setup-custom "setup-buffer-move")

;;
;; Theme specific setup
;;
(setup-custom "setup-theme")

;;
;; Ag specific setup
;;
(setup-custom "setup-ag")

;;
;; Company mode specific setup
;;
(setup-custom "setup-company-mode")

;;
;; Projectile specific setup
;;
(setup-custom "setup-projectile")

;;
;; Clojure specific setup
;;
(setup-custom "setup-clojure")

;;
;; Shell specific setup
;;
(setup-custom "setup-shell")

;;
;; Bash specific setup
;;
(setup-custom "setup-bash")

;;
;; Fish specific setup
;;
(setup-custom "setup-fish")

;;
;; OS X specific setup
;;

(if (string-equal system-type "darwin")
    (setup-custom "setup-osx"))

;;
;; Eldoc mode specific setup
;;
(setup-custom "setup-eldoc")

;;
;; Which key setup
;;
(setup-custom "setup-which-key")

;;
;; Text mode specific setup
;;
(setup-custom "setup-text")

;;
;; Browser specific setup
;;
(setup-custom "setup-browser")

;;
;; Git specific setup
;;
(setup-custom "setup-git")

;;
;; Tramp specific setup
;;
(setup-custom "setup-tramp")

;;
;; Elisp specific setup
;;
(setup-custom "setup-elisp")

;;
;; Ivy specific setup
;;
;; For some reason swiper does not work.
;;
(setup-custom "setup-ivy")

;;
;; Python specific setup
;;
(setup-custom "setup-python")

;;
;; Common Lisp specific setup
;;
(setup-custom "setup-common-lisp")

;;
;; Erlang specific setup
;;
(setup-custom "setup-erlang")

;;
;; Kill current buffer instead of prompting for buffer to kill.
;;
(setup-custom "setup-kill-buffer")

;;
;; Execute shell commands and place the result in the current buffer.
;;
(setup-custom "setup-shell-command")

;;
;; Web mode specific setup
;;
(setup-custom "setup-web-mode")

;;
;; Project navigation specific setup
;;
(setup-custom "setup-project-nav")

;;
;; Treemacs specific setup
;;
(setup-custom "setup-treemacs")

;;
;; Log file specific setup
;;
;;(setup-custom "setup-log")

;;
;; Editor Config specific setup
;;
(setup-custom "setup-editor-config")

;;
;; Flycheck specific setup
;;
(setup-custom "setup-flycheck")

;;
;; SQL specific setup
;;
;; (setup-custom "setup-sql")

;;
;; NGINX specific setup
;;
(setup-custom "setup-nginx")

;;
;; Docker specific setup
;;
(setup-custom "setup-docker")

;;
;; Ascii Doc specific setup
;;
(setup-custom "setup-asciidoc")

;;
;; Markdown specific setup
;;
(setup-custom "setup-markdown")

;;
;; Scheme specific setup
;;
(setup-custom "setup-scheme")

;;
;; Go lang specific setup
;;
(setup-custom "setup-go")

;;
;; YAML specific setup
;;
(setup-custom "setup-yaml")

;;
;; JSON specific setup
;;
(setup-custom "setup-json")

;;
;; JavaScript specific setup
;;
(setup-custom "setup-javascript")

;;
;; Eyebrowse specific setup
;;
(setup-custom "setup-eyebrowse")

;;
;; Todo specific setup
;;
(setup-custom "setup-todo")

;;
;; Multi term setup
;;
(setup-custom "setup-multi-term")

;;
;; Eshell setup
;;
(setup-custom "setup-eshell")

;;
;; Spotify setup
;;
(setup-custom "setup-spotify")

;;
;; html to markdown setup
;;
(setup-custom "setup-html-to-markdown")

;;
;; Miscellaneous customizations
;;
(setup-custom "setup-misc")

;;
;; Libraries for Elisp
;;
(setup-custom "setup-libraries")

;;
;; ERC specific setup
;;
(setup-custom "setup-erc")

;;
;; Guru mode
;;
(setup-custom "setup-guru")

;;
;; Wanderlust email client
;;
;; Right now I cannot load the wanderlust email client from MELPA.
;; I'm going to leave this in there and see if it works later on.
;; (setup-custom "setup-wanderlust.el")

(provide 'init)
;;; init.el ends here
