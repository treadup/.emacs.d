;;; init --- The main emacs user init file.

;;; Commentary:

;;; Code:
;;;

;; The flx-ido readme reccomends increasing the gc-cons-threshold to
;; at least 20 MB.
;; (setq gc-cons-threshold 30000000)

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

(use-package auto-package-update
   :ensure t
   :config

   ;; Update the packages every 4 days
   (setq auto-package-update-interval 4)

   ;; Update all packages on startup that have updates available.
   (auto-package-update-maybe))


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
;; Setup bookmark functions
;;
(setup-custom "setup-bookmarks")

;;
;; Auth source specific setup
;;
(setup-custom "setup-auth-source")

;;
;; Request specific setup
;;
(setup-custom "setup-request")

;;
;; Python virtual environment manager for Eshell
;;
(setup-custom "vpy")

;;
;; Eshell setup
;;
(setup-custom "setup-eshell")

;;
;; Emacs server
;;
(setup-custom "setup-server")

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
;; Unix specific setup
;;
(unless (string-equal system-type "darwin")
  (setup-custom "setup-unix"))

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
;; Python specific setup
;;
(setup-custom "setup-python")

;;
;; Common Lisp specific setup
;;
(setup-custom "setup-common-lisp")

;;
;; Lua specific setup
;;
(setup-custom "setup-lua")

;;
;; Erlang specific setup
;;
(setup-custom "setup-erlang")

;;
;; Elixir specific setup
;;
(setup-custom "setup-elixir")

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
;; NGINX specific setup
;;
(setup-custom "setup-nginx")

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
;; Golang specific setup
;;
(setup-custom "setup-go")

;;
;; Rust specific setup
;;
(setup-custom "setup-rust")

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
;; Todo specific setup
;;
(setup-custom "setup-todo")

;;
;; HTTP client setup
;;
(setup-custom "setup-http")
