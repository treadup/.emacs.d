;;;
;;; Customizations for eshell
;;;

(require 'eshell)

;; The quit function allows us to execute the quit command
;; in eshell to close the eshell buffer and window.
;; Remember that in eshell you can run functions without
;; using parenthesis.

(defun quit ()
"Kill the current buffer and window by typing quit in eshell.
This might be better as an Eshell alias."
  (kill-buffer-and-window))

;; (eshell t) will create a new eshell with the next eshell index number.

;; This one is kind of interesting.
;; (eshell <num>) will do one of two things.
;; If an eshell with index number <num> already exists then switch the
;; window to that buffer.
;; Otherwise create a new eshell with the given index number and show
;; it in the current window.

(defun eshell-below ()
  (interactive)
  (progn
    (split-window-below)
    (other-window 1)
    (eshell t)
    (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)))

(defun eshell-right ()
  (interactive)
  (progn
    (split-window-right)
    (other-window 1)
    (eshell t)
    (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)))

;; Change the prompt we have to set the eshell-prompt-function variable
;; to something that generates a nice prompt. Currently it is very basic.

(defun custom-eshell-prompt-function ()
  "Custom Eshell prompt."
  (concat (abbreviate-file-name (eshell/pwd))
    (if (= (user-uid) 0) " # " " $ ")))

(setq eshell-prompt-function 'custom-eshell-prompt-function)

(provide 'for-eshell.el)
;;; for-eshell ends here
