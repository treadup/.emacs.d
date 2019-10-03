;;; setup-mac --- Customizations for macOS

;;; Commentary:

;;; Code:

(use-package mac-pseudo-daemon
  :ensure t
  :config
  (mac-pseudo-daemon-mode))

;; Use the fn key as the hyper modifier which means that we can now
;; keybind things using H+<key>.
(setq ns-function-modifier 'hyper)

;; Cmd is already the super modifier which means that we can keybind
;; things using S+<key>.

;; Do not popup new frames when opening emacs. Instead open the
;; existing frame.
(setq ns-pop-up-frames nil)

(provide 'setup-mac)
;;; setup-mac.el ends here
