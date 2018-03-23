;;;
;;; Customizations for completion
;;;

;;
;; Ido mode
;;

;; Ido mode allows you to more easily navigate choices.
; (ido-mode t)

;; Use partial matching
; (setq ido-enable-flex-matching t)

;; Enable ido wherever it could be useful.
; (setq ido-everywhere t)

;; Change the way the list of buffers is shown.
;(global-set-key (kbd "C-x C-b") 'ibuffer)

;; The smex package provides an Ido like interface for M-x.
;;(use-package smex
;;  :ensure t
;;  :bind (("M-x" . smex))
;;  :config (smex-initialize))

;;
;; I am not sure if you have to install both ivy and counsel or if
;; counsel pulls in ivy automatically.

;;(use-package counsel
;;  :ensure t)

;; For ido when in find file press C-f to go back to the old behvior.
;; https://stackoverflow.com/questions/17986194/emacs-disable-automatic-file-search-in-ido-mode

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) "))

;;
;; C-n goes to the next match.
;; C-p goes to the previous match.
;; Up and down arrow will move around in the minibuffer.
(global-set-key "\C-s" 'swiper)

;; Resume the last ivy command?
;; Can be used to do a resume search.
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)

(global-set-key (kbd "M-x") 'counsel-M-x)

(global-set-key (kbd "C-x C-f") 'counsel-find-file)

;;
;; Different help commands
;;
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-load-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)

;;
;; Different counsel searching commands
;;

;; Find file in the current git repo.
(global-set-key (kbd "C-c g") 'counsel-git)

;; Grep the current git repo
(global-set-key (kbd "C-c j") 'counsel-git-grep)

;; Use ag to search. (Does this stay within the repo?)
(global-set-key (kbd "C-c k") 'counsel-ag)

;; Use locate to find a file
(global-set-key (kbd "C-x l") 'counsel-locate)
