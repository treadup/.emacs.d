;; This is a bit of a weird one.
;; At work we need to use tabs to indent the code.


;; http://ergoemacs.org/emacs/emacs_tabs_space_indentation_setup.html
(defun personal-insert-tab-char ()
  "Insert a tab char. (ASCII 9, \t)"
  (interactive)
  (insert "\t"))

(defun is-whitespace-string-p (str)
  (eql (string-match "^[ \t]*$" str) 0))

(defun line-beginning-to-point-string ()
  (let ((start (line-beginning-position))
        (end (point)))
    (buffer-substring-no-properties start end)))

(defun whitespace-to-point-p ()
  (let ((linetopoint (line-beginning-to-point-string)))
    (is-whitespace-string-p linetopoint)))

(defun conditional-space-insert-command ()
  (if (whitespace-to-point-p)
      (personal-insert-tab-char)
      (self-insert-command 1)))

(defun conditional-backspace-command ()
  (interactive)
  (if (whitespace-to-point-p)
      (progn
        (delete-backward-char)
        (delete-backward-char))
      (delete-backward-char)))

;; The following keybindings are useful for debugging.
;; (global-set-key (kbd "C-c l") (lambda () (interactive) (message "%s" (line-beginning-to-point-string))))
;; (global-set-key (kbd "C-c w") (lambda () (interactive) (message "%s" (whitespace-to-point-p))))
