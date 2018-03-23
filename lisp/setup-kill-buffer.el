;;;
;;; Customization for kill buffer
;;;

;; Rebind C-x k to 'kill-this-buffer so that you do not get prompted about which
;; buffer you want to kill. Instead the current buffer gets killed.
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;; Bind C-c k to 'kill-this-buffer-and-window
(global-set-key (kbd "C-c k") 'kill-buffer-and-window)
