;;; setup-django --- Customizations for Django

;;; Commentary:

;; Use djangonaut for Django development.
;; https://github.com/proofit404/djangonaut

;; There are a few other packages that can be used for Django development in Emacs.

;; pony-mode
;; https://github.com/davidmiller/pony-mode

;; django-mode
;; https://github.com/myfreeweb/django-mode

;; python-django
;; https://github.com/fgallina/python-django.el

;; Spacemacs uses Pony mode for its Django layer.
;; https://github.com/syl20bnr/spacemacs/tree/master/layers/%2Bframeworks/django

;;; Code:

(use-package djangonaut
  :ensure t
  :config
  (add-hook 'python-mode-hook 'global-djangonaut-mode))

(provide 'setup-django)
;;; setup-django ends here
