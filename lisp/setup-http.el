;;; setup-http --- HTTP helper functions.

;;; Commentary:

;; Some helper functions for working with HTTP in Emacs.

;;; Code:

(require 'url-handlers)

(defun url-insert-body-at-point (url)
  "Download the contents of the URL and insert the body into the buffer at the point."
  (let ((download-buffer (url-retrieve-synchronously url)))
    (url-insert download-buffer)))

(provide 'setup-http)
;;; setup-http ends here
