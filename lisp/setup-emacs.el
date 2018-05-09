;;; setup-emacs --- Configuration of builtin emacs packages.

;;; Commentary:

;;; Code:
;; Automatically revert a buffer if the file was changed by an external
;; program.
;; (global-auto-revert-mode 1)

;;;
;;; Character coding system
;;;

;; This section is from Mastering Emacs. Except I have changed the coding
;; system from utf-8 to utf-8-unix
;; https://www.masteringemacs.org/article/working-coding-systems-unicode-emacs

(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(setq-default buffer-file-coding-system 'utf-8-unix)

;;;
;;; Misc config
;;;

;; Bind M-x to C-RET
(global-set-key (kbd "C-<return>") 'helm-M-x)

;; Bind C-: to eval.
(global-set-key (kbd "C-:") 'eval-expression)

;; Make hyper links clickable in text mode and prog mode.
(add-hook 'text-mode-hook 'goto-address-mode)
(add-hook 'prog-mode-hook 'goto-address-mode)

;; Sentences should end with a single space and not double space.
(setq sentence-end-double-space nil)

;; Calendar weeks should start on monday.
(setq-default calendar-week-start-day 1)

;; Some functions in Emacs are disabled by default.
;; The following will enable individual functions.

;; Enable the erase-buffer function.
(put 'erase-buffer 'disabled nil)

;; When in a window system do not minimize the frame when pressing C-z.
(when window-system (global-unset-key "\C-z"))

;;
;; Buffer
;;

(defun revert-this-buffer ()
  "Revert the current buffer without asking."
  (interactive)
  (revert-buffer nil t t)
  (message (concat "Reverted buffer " (buffer-name))))

;; Make new buffers that are not associated with a file automatically
;; set the major mode based on the buffer name.

(setq-default major-mode
  (lambda () (if buffer-file-name
               (fundamental-mode)
               (let ((buffer-file-name (buffer-name)))
                 (set-auto-mode)))))

;; Buffer key bindings

(global-set-key (kbd "M-n") 'next-buffer)
(global-set-key (kbd "M-p") 'previous-buffer)
(global-set-key (kbd "C-c C-b") 'bury-buffer)

;;
;; Midnight mode
;;

;; Use midnight-mode to get rid of unused buffers.
(require 'midnight)

;; Run midnight-hook 4 hours after midnight
(midnight-delay-set 'midnight-delay (* 4 60 60))

;;
;; Recent files
;;

;; Build a list of recently opened files.
;; https://www.emacswiki.org/emacs/RecentFiles
(customize-set-variable 'recentf-save-file
  (concat user-emacs-directory ".recentf"))
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 40)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

;;
;; Issue: This section uses the custom-set-variables function. This function has to be
;; called before themes and other things are loaded. There is some kind of issue here
;; with the order of the custom-set-variables function being called and functions
;; that use the customize system.
;;

;; To avoid sprinkling backups and auto save files all over the filesystem
;; we can use the following code.
;; https://snarfed.org/gnu_emacs_backup_files

;;
;; Autosave
;;

;; Put autosave files (ie #foo#) and backup files (ie foo~) in ~/.emacs.d/.
(customize-set-variable 'auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))

;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)

;;
;; Backup
;;

(customize-set-variable 'backup-directory-alist '((".*" . "~/.emacs.d/backups/")))

;; Now we have some other backup related stuff.
;; For a description of how these variables work see the following url
;; http://stackoverflow.com/questions/151945/how-do-i-control-how-emacs-makes-backup-files

;; Make backups by copying
(setq backup-by-copying t)

;; Use version numbers on backups
(setq version-control t)

;; Delete old versions. In other words do not keep excess backups.
(setq delete-old-versions t)

;; Keep 3 of the newest versions of the file.
(setq kept-new-versions 3)

;; Keep 5 of the oldest versions of the file.
(setq kept-old-versions 3)

;; Create backups of version controlled files.

(customize-set-variable 'vc-make-backup-files t)

(provide 'setup-emacs)
;;; setup-emacs.el ends here
