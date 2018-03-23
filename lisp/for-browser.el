;;;
;;; Customization for web browser
;;;

;; The browse-url package is part of emacs but we still have to require it.
(require 'browse-url)

;; It seems like you have to enter http:// infront of the url. Otherwise the
;; browse-url command will not do anything.
;; (browse-url) will prompt you for the URL interactively.
;; (browse-url "http://the.url.goes.here/some/path") will navigate to the url directly.
(global-set-key (kbd "C-c C-o") (lambda () (interactive) (browse-url-at-point)))

;; (browse-url-at-point) will browse the url at the point.
;; C-c C-o should be bound to browse-url-at-point. 

;; Using the browse-url function I should be able to do something that is similar to
;; the Mac program Alfred.
;;
;; (defun goog)
;; (defun google)
;; (defun amz)
;; (defun amazon)
;; (defun red)


