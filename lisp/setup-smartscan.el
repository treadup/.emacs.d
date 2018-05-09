;;; setup-smartscan --- Setup smartscan

;;; Commentary:

;; With smartscan we can navigate between the symbols found at point.
;; M-n and M-p move between symbols.
;; M-' to replace all symbols in the buffer matching the one under point.

;;; Code:
(use-package smartscan
  :ensure t
  :config
  (global-smartscan-mode))

(provide 'setup-smartscan)
;;; setup-smartscan ends here
