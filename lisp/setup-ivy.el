;;;
;;; Customizations for Ivy
;;;

;;
;; Ivy
;;

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")

  ;; Use C-j for immediate termination with the current value, and RET
  ;; for continuing completion for that directory. This is the ido
  ;; behaviour.
  ;; https://github.com/abo-abo/swiper/wiki/ido-style-folder-navigation
  (define-key ivy-minibuffer-map (kbd "C-j") #'ivy-immediate-done)
  (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done))

;;
;; Swiper
;;
(use-package swiper
  :ensure t
  :config
  (progn
    (global-set-key "\C-s" 'swiper)))

;;
;; Counsel
;;
(use-package counsel
  :ensure t
  :config
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file))

;;
;; C-n goes to the next match.
;; C-p goes to the previous match.
;; Up and down arrow will move around in the minibuffer.
;; (global-set-key "\C-s" 'swiper)

;; Resume the last ivy command?
;; Can be used to do a resume search.
;;(global-set-key (kbd "C-c C-r") 'ivy-resume)
;;(global-set-key (kbd "<f6>") 'ivy-resume)

;; (global-set-key (kbd "M-x") 'counsel-M-x)

;; (global-set-key (kbd "C-x C-f") 'counsel-find-file)

;;
;; Different help commands
;;
;; (global-set-key (kbd "<f1> f") 'counsel-describe-function)
;; (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
;; (global-set-key (kbd "<f1> l") 'counsel-load-library)
;; (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
;; (global-set-key (kbd "<f2> u") 'counsel-unicode-char)

;;
;; Different counsel searching commands
;;

;; Find file in the current git repo.
;; (global-set-key (kbd "C-c g") 'counsel-git)

;; Grep the current git repo
;; (global-set-key (kbd "C-c j") 'counsel-git-grep)

;; Use ag to search. (Does this stay within the repo?)
;; (global-set-key (kbd "C-c k") 'counsel-ag)

;; Use locate to find a file
;; (global-set-key (kbd "C-x l") 'counsel-locate)

;; Not sure if the following will work.
;; (setq counsel-find-file-ignore-regexp "\\.elc\\'")
