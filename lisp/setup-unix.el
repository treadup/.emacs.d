;;; setup-unix --- Unix specific setup goes here

;;; Commentary:
;; When running in graphical mode on Linux do not quit Emacs when
;; the C-x C-c key chord is pressed. Instead just suspend the frame.
;; This is preferable to running Emacs in daemon mode since with
;; this approach Emacs will correctly interact with the dock.

;;; Code:

(defun suspend-graphical-frame-kill-terminal-frame ()
  "Suspend the frame if it is a graphical frame.
Kill the frame if it is a terminal frame."
  (interactive)
  (if (display-graphic-p)
      (suspend-frame)
      (save-buffers-kill-terminal)))

;; Suspend the current emacs frame instead of killing Emacs
(global-set-key (kbd "C-x C-c") 'suspend-graphical-frame-kill-terminal-frame)

(provide 'setup-unix)
;;; setup-unix ends here
