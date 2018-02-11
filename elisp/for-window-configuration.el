;;;
;;; Customizations for window configuration
;;;

;; With winner mode we can undo and redo changes to the
;; window configuration.
;;
;; To undo use C-c <left arrow>
;; To redo use C-c <right arrow>
(winner-mode 1)

;; Key bindings for Emacs built in window resizing functions.
;; Shift Ctrl <arrow key> performs the following actions.
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; The above works but is no intuitive. Instead try to find some way of
;; moving the border around. Here is some code that does that.
;; But the copyright is unclear.
;; https://vickychijwani.me/nuggets-from-my-emacs-part-i/

;; Windmove lets you move the point between windows using Shift and the
;; arrow keys.
(windmove-default-keybindings)
