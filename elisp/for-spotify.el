;;;
;;; Customizations for spotify
;;;

(use-package spotify
  :ensure t
  :config
  (global-set-key (kbd "C-c m s") 'spotify-playpause)
  (global-set-key (kbd "C-c m n") 'spotify-previous)
  (global-set-key (kbd "C-c m p") 'spotify-next))

;; Emacs has a built in music player called MPC. The above keybindings
;; are inspired by the MPC keybindings.

(provide 'for-spotify)
;;; for-spotify.el ends here
