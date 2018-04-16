;;; setup-buffer-move --- Customizations for buffer move

;;; Commentary:

;;; Code:

(use-package buffer-move
  :ensure t
  :config
  (global-set-key (kbd "C-S-<up>")     'buf-move-up)
  (global-set-key (kbd "C-S-<down>")   'buf-move-down)
  (global-set-key (kbd "C-S-<left>")   'buf-move-left)
  (global-set-key (kbd "C-S-<right>")  'buf-move-right))

(provide 'setup-buffer-move)
;;; setup-buffer-move.el ends here
