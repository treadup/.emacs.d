;;; setup-helm --- Configuration for Helm

;;; Commentary:

;; Helm takes a while to load.  Because of this you might want to defer
;; loading of helm until Emacs is idle.  However this is hard to do using
;; the use-package macro.  The approach that we will use is to just load
;; helm using use-package and accept the increased load time.

;; The helm-mini command shows a dashboard.  You can customize the contents
;; of the dashboard by setting certain variables.

;; For searching I might want to use helm-occur or helm-swiper.  There is
;; also helm swoop.

;; Might want to use the helm-flx package for better fuzzy matching.

;;; Code:

(use-package helm
  :diminish helm-mode
  :ensure t
  :bind
  (("C-x C-f" . helm-find-files)
    ("M-x" . helm-M-x)
    ("M-y" . helm-show-kill-ring)
    ("C-x C-b" . helm-buffers-list)
    ("C-h a" . helm-apropos)
    ("C-h m" . helm-man-woman)
    ("C-h r" . helm-info-emacs)
    ("C-h t" . helm-world-time)
    ("C-x r l" . 'helm-bookmarks))

  :config
  (require 'helm-config)
  ;; By default this is C-x c which is really close to C-x C-c which will exit Emacs.
  ;; Change the helm prefix key to be C-c h instead.
  (customize-set-variable 'helm-command-prefix-key "C-c h")

  ;; Change behavior of tab.
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
  (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

  ;; Set which times helm-world-time should display
  (setq display-time-world-list '(("Europe/Stockholm" "Stockholm")
                                   ("America/Los_Angeles" "San Diego")
                                   ("Pacific/Honolulu" "Honolulu")
                                   ;; Should find the timezone for Wyoming as well.
                                   ))
  ;; Make helm-find-file skip boring files like .pyc and .pyo files.
  (setq helm-ff-skip-boring-files t)

  (helm-mode 1))

(use-package helm-ag
  :ensure t)

(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on))

;; Use swiper for searching the buffer.
(use-package swiper-helm
  :ensure t
  :bind (("C-s" . swiper-helm)
         ("C-r" . swiper-helm)))

(provide 'setup-helm)
;;; setup-helm.el ends here
