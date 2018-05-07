;;; setup-request --- Configuration for request

;;; Commentary:

;; The request.el package allows you to make http requests.
;; It will use curl when curl is available otherwise it will
;; fallback on url.el
;; http://tkf.github.io/emacs-request/

;;; Code:
(use-package request
  :ensure t)

(provide 'setup-request)
;;; setup-request ends here
