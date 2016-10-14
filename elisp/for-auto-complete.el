;;;
;;; Customizations for Auto Complete
;;;

;; dirty fix for having AC everywhere
;; from EmacsWiki
;; Probably do not want auto complete in text mode.
(define-globalized-minor-mode real-global-auto-complete-mode
  auto-complete-mode (lambda ()
                       (if (not (minibufferp (current-buffer)))
                         (auto-complete-mode 1))
                       ))

(defun personal-auto-complete-setup ()
  (setq-default ac-sources '(ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))
  ;; (global-auto-complete-mode t)
  ;; (real-global-auto-complete-mode t)
  (add-hook 'prog-mode-hook 'auto-complete-mode))

(use-package auto-complete
  :ensure t
  :config
  (personal-auto-complete-setup))
