;; This is a bit of a weird one.
;; At work we need to use tabs to indent the code.

(defun is-whitespace-string-p (str)
  (eql (string-match "^[ \t]*$" str) 0))

(defun line-beginning-to-point-string ()
  (let ((start (line-beginning-position))
        (end (point)))
    ;; (message "start = %d, end = %d" start end)
    (buffer-substring-no-properties start end)))

(defun whitespace-to-point-p ()
  (let ((linetopoint (line-beginning-to-point-string)))
    (is-whitespace-string-p linetopoint)))

(defun conditional-space-insert-command ()
  (message "Spacebar pressed")
  (unless (whitespace-to-point-p)
    (self-insert-command 1)))

;; The following keybindings are useful for debugging.
;; (global-set-key (kbd "C-c l") (lambda () (interactive) (message "%s" (line-beginning-to-point-string))))
;; (global-set-key (kbd "C-c w") (lambda () (interactive) (message "%s" (whitespace-to-point-p))))

;; Do not do this as a global set key. Blocking the spacebar should be set on a per mode basis.
;; (global-set-key (kbd "SPC") (lambda () (interactive) (conditional-space-insert-command)))
