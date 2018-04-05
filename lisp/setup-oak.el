;;;
;;; Customizations for oak
;;;

(defun edit-bookmarks ()
  "Edit bookmarks on oak"
  (interactive)
  (find-file "/oak:~/code/python/redwood/data/bookmarks.json"))

(defun notes-eshell ()
"Open the notes folder in eshell locally on redwood and on oak
otherwise"
  (interactive)
  (let ((default-directory
          (unless (is-host-p "hickory")
            "~/notes"
            "/oak:~/notes")))
    (eshell)))
