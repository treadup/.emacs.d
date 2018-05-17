;;; init --- The main emacs user init file.

;;; Commentary:

;;; Code:
;;;

;; The flx-ido readme reccomends increasing the gc-cons-threshold to
;; at least 20 MB.
(setq gc-cons-threshold 30000000)

;; Byte compile all files in the .emacs.d/lisp folder. Force byte compilation
;; even if there is no existing .elc file for the .el file.
;; (byte-recompile-directory "~/.emacs.d/lisp" 0)

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

;; This is required for some reason on macOS.
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

;;
;; Load path
;;

;; Store custom elisp files in the /elisp subdirectory of the dot emacs folder.
;; https://www.emacswiki.org/emacs/DotEmacsDotD
(add-to-list 'load-path (concat user-emacs-directory (convert-standard-filename "lisp/")))

(defun setup-custom (filename)
  "Load custom settings from the file with name FILENAME."
;;  (message (concat "Loading " filename))
  (load filename))

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

;;;
;;; Customizations
;;;

;;
;; Emacs specific setup
;;
(setup-custom "setup-emacs")

;;
;; Exec path
;;
(setup-custom "setup-exec-path")

;;
;; Libraries for Elisp
;;
(setup-custom "setup-libraries")

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
;; Request specific setup
;;
(setup-custom "setup-request")

;;
;; Hydra specific setup
;;
(setup-custom "setup-hydra")

;;
;; Python virtual environment manager for Eshell
;;
(setup-custom "vpy")

;;
;; Django specific setup
;;
(setup-custom "setup-django")

;;
;; Eshell setup
;;
(setup-custom "setup-eshell")

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
;; Setup editing with Emacs
;;
(setup-custom "setup-editing")

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
;; Clojure specific setup
;;
(setup-custom "setup-clojure")

;;
;; Shell specific setup
;;
(setup-custom "setup-shell")

;;
;; Xterm color setup
;;
(setup-custom "setup-xterm-color")

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
    (setup-custom "setup-mac"))

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
;; (setup-custom "setup-ivy")

;;
;; Helm specific setup
;;
(setup-custom "setup-helm")

;;
;; Ido specific setup
;;
;; (setup-custom "setup-ido")

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
;; PHP specific setup
;;
(setup-custom "setup-php")

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
;; Flyspell specific setup
;;
(setup-custom "setup-flyspell")

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
;; Vimrc specific setup
;;
(setup-custom "setup-vimrc")

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
;; Magit setup
;;
(setup-custom "setup-magit")

;;
;; HTTP client setup
;;
(setup-custom "setup-http")

;;
;; Realgud setup
;;
(setup-custom "setup-realgud")

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
;; ERC specific setup
;;
(setup-custom "setup-erc")

;;
;; Guru mode
;;
(setup-custom "setup-guru")

;;
;; Yasnippet
;;
(setup-custom "setup-yasnippet")

;;
;; Pass specific setup
;;
(setup-custom "setup-pass")

;;
;; Ranger specific setup
;;
(setup-custom "setup-ranger")

;;
;; elfeed specific setup
;;
(setup-custom "setup-elfeed")

;;
;; Smartscan specific setup
;;
(setup-custom "setup-smartscan")

;;
;; Host specific setup
;;
(setup-custom "setup-oak")

;;
;; Work specific setup
;;
(when (work-context-p)
  (setup-custom "setup-work"))

;;
;; Home computer specific setup
;;
(when (home-context-p)
  (setup-custom "setup-home"))

;;
;; Wanderlust email client
;;
(setup-custom "setup-wanderlust.el")

(provide 'init)
;;; init.el ends here
