;;; setup-avy --- Customizations for Avy

;;; Commentary:

;; Jump to visible text using Avy.
;; https://github.com/abo-abo/avy

;; http://pragmaticemacs.com/emacs/super-efficient-movement-using-avy/
;; http://emacsredux.com/blog/2015/07/19/ace-jump-mode-is-dead-long-live-avy/

;;; Code:
(use-package avy
  :ensure t
  :bind (("C-M-s" . avy-goto-word-1)))

(provide 'setup-avy)
;;; setup-avy ends here
