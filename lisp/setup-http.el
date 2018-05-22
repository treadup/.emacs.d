;;; setup-http --- HTTP helper functions.

;;; Commentary:

;; Some helper functions for working with HTTP in Emacs.

;;; Code:

(require 'url-handlers)

;; TODO: See if we can change this to being async.

(defun url-insert-body-at-point (url)
  "Download the contents of the URL and insert the body into the buffer at the point."
  (let ((download-buffer (url-retrieve-synchronously url)))
    (url-insert download-buffer)))

(defun url-replace-body (url)
  "Replace the current buffers contents with the body of the URL.
If update-file-name is true then the UPDATE-FILE-NAME of the buffer
is set to the given URL."
  (erase-buffer)
  (url-insert-body-at-point url))

(defun browse(url)
  "Browse the given URL in a new buffer.
Creates a new buffer whose contents is the body of the given URL.
The buffer name is set to the URL."
  (with-current-buffer (get-buffer-create url)
    (url-replace-body url)
    (switch-to-buffer (current-buffer))
    (web-mode)))

;; Key bind C-c r to browsing the URL at point. Or enter a url
;; manually if there is no url at point.


(provide 'setup-http)
;;; setup-http ends here
