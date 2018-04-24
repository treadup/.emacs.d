;;; setup-eyebrowse --- Customizations for eyebrowse

;;; Commentary:

;; For now use Eyebrowse for workspace management.

;; Another package that it is worth looking into is persp-mode.
;; https://github.com/Bad-ptr/persp-mode.el
;; https://github.com/bbatsov/persp-projectile

;; Documentation on the eyebrowse package can be found
;; at the following location.
;; https://github.com/wasamasa/eyebrowse

;; I think that rebinding C-M-0, C-M-1, ..., C-M-9 to window management makes sense.
;; C-M-< and C-M-> are currently free.
;; C-M-' and C-M-" are currently free.
;; C-M-, is currently free.

;; Here are the current keybindings
;; C-c C-w <	Switch to previous window config
;; C-c C-w >	Switch to next window config
;; C-c C-w '	Switch to last window config
;; C-c C-w "	Close current window config
;; C-c C-w ,	Rename current window config
;; C-c C-w 0	Switch to window config 0
;; ...	...
;; C-c C-w 9	Switch to window config 9

;; I am going to use C-M as the prefix for the eyebrowse commands.
;; C-M-<	Switch to previous window config
;; C-M->	Switch to next window config
;; C-M-'	Switch to last window config
;; C-M-"	Close current window config
;; C-M-,	Rename current window config
;; C-M-0	Switch to window config 0
;; ...	...
;; C-M-9	Switch to window config 9

;;; Code:
(use-package eyebrowse
  :ensure t
  :config

  ;; Enable eyebrowse everywhere
  (eyebrowse-mode t)

  ;; Unbind the old C-M-n keybindings.
  (dotimes (n 10)
    (global-unset-key (kbd (format "C-M-%d" n))))

  ;; Set new keybindings for switching window config.
  (global-set-key (kbd "C-M-0") 'eyebrowse-switch-to-window-config-0)
  (global-set-key (kbd "C-M-1") 'eyebrowse-switch-to-window-config-1)
  (global-set-key (kbd "C-M-2") 'eyebrowse-switch-to-window-config-2)
  (global-set-key (kbd "C-M-3") 'eyebrowse-switch-to-window-config-3)
  (global-set-key (kbd "C-M-4") 'eyebrowse-switch-to-window-config-4)
  (global-set-key (kbd "C-M-5") 'eyebrowse-switch-to-window-config-5)
  (global-set-key (kbd "C-M-6") 'eyebrowse-switch-to-window-config-6)
  (global-set-key (kbd "C-M-7") 'eyebrowse-switch-to-window-config-7)
  (global-set-key (kbd "C-M-8") 'eyebrowse-switch-to-window-config-8)
  (global-set-key (kbd "C-M-9") 'eyebrowse-switch-to-window-config-9)

  ;; Switch to next and previous window config.
  (global-set-key (kbd "C-M->") 'eyebrowse-next-window-config)
  (global-set-key (kbd "C-M-<") 'eyebrowse-prev-window-config)

  ;; Rename window config
  (global-set-key (kbd "C-M-,") 'eyebrowse-rename-window-config)

  ;; Switch to last window config
  (global-set-key (kbd "C-M-'") 'eyebrowse-last-window-config)

  ;; Close current window config
  (global-set-key (kbd "C-M-\"") 'eyebrowse-close-window-config))

(provide 'setup-eyebrowse)
;;; setup-eyebrowse.el ends here
