;;; setup-osx --- Customizations for OS X

;;; Commentary:
;;
;; Implement cut and paste for OSX.
;; https://apple.stackexchange.com/questions/85222/configure-emacs-to-cut-and-copy-text-to-mac-os-x-clipboard


;;; Code:

;;
;; Cut and Paste
;;

(defun pbcopy ()
  (interactive)
  (call-process-region (point) (mark) "pbcopy")
  (setq deactivate-mark t))

(defun pbpaste ()
  (interactive)
  (call-process-region (point) (if mark-active (mark) (point)) "pbpaste" t t))

(defun pbcut ()
  (interactive)
  (pbcopy)
  (delete-region (region-beginning) (region-end)))

(global-set-key (kbd "C-c c") 'pbcopy)
(global-set-key (kbd "C-c v") 'pbpaste)
(global-set-key (kbd "C-c x") 'pbcut)

(provide 'setup-osx)
;;; setup-osx.el ends here
