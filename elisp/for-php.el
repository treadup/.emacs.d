;;;
;;; Customizations for PHP
;;;
;;; This file contains things that are language specific to PHP.
;;;

;;
;; PHP
;;
(defun personal-php-indent ()
    (tabify (point-min) (point-max)))
  
(defun personal-php-mode-config ()
    (message "Applying personalized PHP configuration")

    ;; We use make-local-variable below so that the settings
    ;; only apply to the local buffer.
    
    ;; Indent using tab characters.
    (make-local-variable 'indent-tabs-mode)
    (setq indent-tabs-mode t)
    
    ;; Use two character indent when indenting PHP code.
    (make-local-variable 'c-basic-offset)
    (setq c-basic-offset 2)

    ;; The width of the tab should be 2 characters.
    (make-local-variable 'tab-width)
    (setq tab-width 2)
    
    ;; Add hook to indent PHP code before saving.
    ;; The last argument is called local. Settin local to t means
    ;; that this hook is a buffer-local hook.
    ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Setting-Hooks.html
    (add-hook 'before-save-hook 'personal-php-indent nil t))

(use-package php-mode
  :ensure t
  :defer t
  :config
  (add-hook 'php-mode-hook 'personal-php-mode-config))



;;    (add-hook 'before-save-hook 
;;          (lambda () (untabify (point-min) (point-max))))))

;; I should be using tabs with the width 2.
;; Indent tabs mode should be on.
;; https://www.emacswiki.org/emacs/UntabifyUponSave
;; https://www.gnu.org/software/emacs/manual/html_node/eintr/Indent-Tabs-Mode.html
