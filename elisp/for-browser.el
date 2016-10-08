;;;
;;; Customization for web browser
;;;

;; The browse-url package is part of emacs but we still have to require it.
(require 'browse-url)

;; It seems like you have to enter http:// infront of the url. Otherwise the
;; browse-url command will not do anything.

;; (setq browse-url-browser-function 'browse-url-firefox)
