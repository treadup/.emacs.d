;;;
;;; Customizations for PHP
;;;
;;; This file contains things that are language specific to PHP.
;;;

;;
;; PHP
;;

(defun personal-php-mode-config ()
    (message "Applying personalized PHP configuration")
    (make-local-variable 'indent-tabs-mode)
    (make-local-variable 'c-basic-offset)
    (make-local-variable 'tab-width)
    (setq c-basic-offset 2)
    (setq tab-width 2)
    (setq indent-tabs-mode t))

(use-package php-mode
  :ensure t
  :config
  (add-hook 'php-mode-hook 'personal-php-mode-config))



;;    (add-hook 'before-save-hook 
;;          (lambda () (untabify (point-min) (point-max))))))

;; I should be using tabs with the width 2.
;; Indent tabs mode should be on.
;; https://www.emacswiki.org/emacs/UntabifyUponSave
;; https://www.gnu.org/software/emacs/manual/html_node/eintr/Indent-Tabs-Mode.html
