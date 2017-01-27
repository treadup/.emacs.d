;;;
;;; Work Configuration
;;;

(message "Entering for-work-configuration.el")

;; http://ergoemacs.org/emacs/emacs_tabs_space_indentation_setup.html
(defun personal-insert-tab-char ()
  "Insert a tab char. (ASCII 9, \t)"
  (interactive)
  (insert "\t"))

(defun is-special-project-buffer ()
  "Predicate that determines if the current buffer is from a special project."
    (cond
     ((not (buffer-file-name)) nil)
     ((string-prefix-p "/Users/henrik/code/booli.se-api" (buffer-file-name)) t)
     ((string-prefix-p "/Users/henrik/code/booli_pro" (buffer-file-name)) t)
     (t nil)))

(defun special-work-project-setup ()
  "Setup for special work projects"
  (message "Applying special work configuration.")
  (electric-indent-mode -1)
  (setq indent-tabs-mode -1)
  (setq tab-width 4)
  (local-set-key (kbd "<tab>") 'personal-insert-tab-char)
  (local-set-key (kbd "<backspace>") 'delete-backward-char)
  (local-set-key (kbd "SPC") (lambda () (interactive) (conditional-space-insert-command))))

(defun custom-work-configuration ()
  (message "About to determine if we need the special work configuration.")
  (message (concat "Current buffer filename: " (buffer-file-name)))
  (if (is-special-project-buffer)
      (progn
          (message "Special work configuration is needed.")
          (special-work-project-setup))
      (message "Special work configuration is not needed.")))

(message "Adding hook for custom-work-configuration")
(add-hook 'prog-mode-hook (lambda() (custom-work-configuration)))
