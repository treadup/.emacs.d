;;;
;;; Customization for SunriseCommander
;;;

(use-package sunrise-commander
  :ensure t)


;; https://www.emacswiki.org/emacs/Sunrise_Commander_Tips
(defun sr-reset-panes ()
      "Hard-reset SC panes."
      (interactive)
      (when sr-running (sr-setup-windows)))


