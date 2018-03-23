;;;
;;; Customizations for treemacs
;;;

;; Treemacs is a file tree for Emacs. There are two packages that are
;; relevant namely treemacs and treemacs-projectile.

;; The main github repo for Treemacs contains a good README.
;; https://github.com/Alexander-Miller/treemacs

;; This is the spacemacs configuration for treemacs.
;; Could provide some inspiration for how to use this.
;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/%2Bfiletree/treemacs

;; The following is from the main Treemacs README. What is left to figure
;; out are the keybindings that I want to use for both treemacs and treemacs-projectile.

;; Treemacs also has the concept of tagging. Should look
;; into how tagging works.

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-change-root-without-asking nil
          treemacs-collapse-dirs              (if (executable-find "python") 3 0)
          treemacs-file-event-delay           5000
          treemacs-follow-after-init          t
          treemacs-follow-recenter-distance   0.1
          treemacs-goto-tag-strategy          'refetch-index
          treemacs-indentation                2
          treemacs-indentation-string         " "
          treemacs-is-never-other-window      nil
          treemacs-never-persist              nil
          treemacs-no-png-images              nil
          treemacs-recenter-after-file-follow nil
          treemacs-recenter-after-tag-follow  nil
          treemacs-show-hidden-files          t
          treemacs-silent-filewatch           nil
          treemacs-silent-refresh             nil
          treemacs-sorting                    'alphabetic-desc
          treemacs-tag-follow-cleanup         t
          treemacs-tag-follow-delay           1.5
          treemacs-width                      35)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)

    ;; Possible values are 'simple and 'extended
    (treemacs-git-mode 'simple))

  :bind (:map global-map
        ;; These two are fine
        ([f8]         . treemacs-toggle)
        ("C-c 1"      . treemacs-delete-other-windows)

        ;; After here we need to start thinking about how we
        ;; want to map things.
        ("C-c b t"     . treemacs-toggle)
        ("C-c b T"     . treemacs)
;;        ("M-m fB"     . treemacs-bookmark)
;;        ("M-m f C-t"  . treemacs-find-file)
;;        ("M-m f M-t"  . treemacs-find-tag)
        ))

(use-package treemacs-projectile
  :defer t
  :ensure t
  :config
  (setq treemacs-header-function #'treemacs-projectile-create-header)
  :bind (:map global-map
              ("C-c b P" . treemacs-projectile)
              ("C-c b p" . treemacs-projectile-toggle)))

;; treemacs and treemacs-projectile are similar
;; treemacs-toggle and treemacs-projectile-toggle are also similar
;; Treemacs seems most useful when you are in a project.

;; What is the difference between the commands treemacs and treemacs-toggle?
