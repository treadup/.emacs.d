;;;
;;; Customizations for Git
;;;

(use-package git-gutter
  :ensure t
  :config (global-git-gutter-mode 1))

(global-set-key (kbd "C-x g") 'magit-status)
;; https://magit.vc/manual/magit/Getting-started.html#Getting-started
