;;; setup-git --- Customizations for Git

;;; Commentary:

;; Unfortunately git-gutter mode interferes with linum mode. You cannot currently
;; use both at the same time. Line numbers seem to be more important so I will enable
;; linum mode and disable git gutter for now.

;;; Code:

;;
;; Show git gutter
;;

(when (>= emacs-major-version 26)
  (use-package git-gutter
    :ensure t
    :config
    (progn
      (add-hook 'prog-mode-hook 'git-gutter-mode)
      (add-hook 'text-mode-hook 'git-gutter-mode))))

(global-set-key (kbd "C-x g") 'magit-status)
;; https://magit.vc/manual/magit/Getting-started.html#Getting-started

(provide 'setup-git)
;;; setup-git ends here
