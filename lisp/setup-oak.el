;;; setup-oak --- Customizations for oak

;;; Commentary:

;;; Code:

(defun oak-bookmarks ()
  "Edit bookmarks on oak."
  (interactive)
  (find-file "/oak:~/code/python/redwood/data/bookmarks.json"))

(defun oak-notes ()
"Open the notes folder in eshell locally on oak."
  (interactive)
  (let ((default-directory "/oak:~/notes"))
    (eshell t)))

(defun oak ()
"Open an Eshell on oak."
  (interactive)
  (let ((default-directory "/oak:~/"))
    (eshell t)))

(provide 'setup-oak)
;;; setup-oak ends here
