;;; setup-http --- Configuration related to http

;;; Commentary:

;;; Code:

(defun fetch (url)
  "Fetch the given URL and place the contents in the current buffer."
   (with-current-buffer (url-retrieve-synchronously url)
     ;; Remove the http headers.
     ;; (search-forward "\n\n")
     ;; (delete-region (point-min) (point))

     ;; We want this to go in the buffer and not the minibuffer.

     (buffer-string)))


(provide 'setup-http)
;;; setup-http ends here
