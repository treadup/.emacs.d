;;;
;;; Customizations for Git
;;;

;; Unfortunately git-gutter mode interferes with linum mode. You cannot currently
;; use both at the same time. Line numbers seem to be more important so I will enable
;; linum mode and disable git gutter for now.

;; (use-package git-gutter
;;  :ensure t)
;;
;; Show git gutter
;; (add-hook 'prog-mode-hook 'git-gutter-f)
;; (add-hook 'text-mode-hook 'git-gutter-mode)

(global-set-key (kbd "C-x g") 'magit-status)
;; https://magit.vc/manual/magit/Getting-started.html#Getting-started
