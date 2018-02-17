;;;
;;; Customizations for context
;;;

(defun work-context-p ()
  "Determines if we are running on a work computer."
  (is-host-p "hickory"))

(defun home-context-p ()
  "Determines if we are running on a home computer."
  (or
    (is-host-p "redwood")
    (is-host-p "oak")))
