;;; setup-emacs --- Configuration of builtin emacs packages.

;;; Commentary:

;;; Code:
;; Automatically revert a buffer if the file was changed by an external
;; program.
;; (global-auto-revert-mode 1)

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

(provide 'setup-emacs)
;;; setup-emacs.el ends here
