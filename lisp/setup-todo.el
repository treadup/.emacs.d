;;; setup-todo --- Customizations for todo
;;; Commentary:

;;; Code:

(defun work-todo ()
  "Open the work-todo.org buffer.
Open the ~/work/work-todo.org file if the buffer is not already open."
  (interactive)
  (find-file "~/work/work-todo.org"))

;; Define a function that toggles between the work-todo file and the
;; current file. Kind of like how C-x 7 is handled currently.

(defun toggle-work-todo ()
  "Toggle between a buffer and the work-todo.org buffer/file."
  (interactive)
  (if (equal (buffer-name) "work-todo.org")
    (switch-to-buffer (other-buffer))
    (work-todo)))

(global-set-key (kbd "C-x 8") 'toggle-work-todo)

(provide 'setup-todo)
;;; setup-todo ends here
