;;; setup-git --- Customizations for Git

;;; Commentary:

;; Unfortunately git-gutter mode interferes with linum mode. You cannot currently
;; use both at the same time. Line numbers seem to be more important so I will enable
;; linum mode and disable git gutter for now.

;;; Code:

;;
;; Show git gutter
;;

(use-package git-gutter
  :ensure t
  :config
  (progn
    (add-hook 'prog-mode-hook 'git-gutter-mode)
    (add-hook 'text-mode-hook 'git-gutter-mode)))

(use-package magit
  :ensure
  :config
  (global-set-key (kbd "C-x g") 'magit-status))

(provide 'setup-git)
;;; setup-git ends here
