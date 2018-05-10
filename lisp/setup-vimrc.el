;;; setup-vimrc --- Setup vimrc mode

;;; Commentary:

;;; https://github.com/mcandre/vimrc-mode/

;;; Code:

(use-package vimrc-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.vim\\(rc\\)?\\'" . vimrc-mode)))

(provide 'setup-vimrc)
;;; setup-vimrc ends here
