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

;; To update the packages (or is it the index of packages?) run (package-refresh-contents)
;; You can also run M-x package-list-packages. This will also update the list of packages.

;;
;; Load path
;;

;; Store custom elisp files in the /elisp subdirectory of the dot emacs folder.
;; https://www.emacswiki.org/emacs/DotEmacsDotD
(add-to-list 'load-path (concat user-emacs-directory (convert-standard-filename "lisp/")))

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
(load "setup-emacs")

;;
;; Exec path
;;
(load "setup-exec-path")

;;
;; Hostname functions
;;
(load "setup-hostname")

;;
;; Setup bookmark functions
;;
(load "setup-bookmarks")

;;
;; Eshell setup
;;
(load "setup-eshell")

;;
;; Emacs server
;;
(load "setup-server")

;;
;; Rainbow delimiters
;;
(load "setup-rainbow-delimiters")

;;
;; User interface specific setup
;;
(load "setup-ui")

;;
;; Window numbering
;;
(load "setup-winum")

;;
;; Window Configuration
;;
(load "setup-window-configuration")

;;
;; Setup editing with Emacs
;;
(load "setup-editing")

;;
;; Theme specific setup
;;
(load "setup-theme")

;;
;; Ag specific setup
;;
(load "setup-ag")

;;
;; Clojure specific setup
;;
(load "setup-clojure")

;;
;; Shell specific setup
;;
(load "setup-shell")

;;
;; Xterm color setup
;;
(load "setup-xterm-color")

;;
;; Bash specific setup
;;
(load "setup-bash")

;;
;; Fish specific setup
;;
(load "setup-fish")

;;
;; OS X specific setup
;;

(if (string-equal system-type "darwin")
    (load "setup-mac"))

;;
;; Unix specific setup
;;
(unless (string-equal system-type "darwin")
  (load "setup-unix"))

;;
;; Eldoc mode specific setup
;;
(load "setup-eldoc")

;;
;; Which key setup
;;
(load "setup-which-key")

;;
;; Text mode specific setup
;;
(load "setup-text")

;;
;; Tramp specific setup
;;
(load "setup-tramp")

;;
;; Python specific setup
;;
(load "setup-python")

;;
;; Common Lisp specific setup
;;
(load "setup-common-lisp")

;;
;; Lua specific setup
;;
;; (load "setup-lua")

;;
;; Erlang specific setup
;;
;; (load "setup-erlang")

;;
;; Elixir specific setup
;;
;; (load "setup-elixir")

;;
;; Execute shell commands and place the result in the current buffer.
;;
(load "setup-shell-command")

;;
;; Web mode specific setup
;;
(load "setup-web-mode")

;;
;; Editor Config specific setup
;;
(load "setup-editor-config")

;;
;; Flycheck specific setup
;;
(load "setup-flycheck")

;;
;; Flyspell specific setup
;;
(load "setup-flyspell")

;;
;; Markdown specific setup
;;
(load "setup-markdown")

;;
;; Scheme specific setup
;;
(load "setup-scheme")

;;
;; Golang specific setup
;;
(load "setup-go")

;;
;; YAML specific setup
;;
(load "setup-yaml")

;;
;; JSON specific setup
;;
(load "setup-json")

;;
;; JavaScript specific setup
;;
(load "setup-javascript")

;;
;; Todo specific setup
;;
(load "setup-todo")

(provide 'init)
;;; init.el ends here.
