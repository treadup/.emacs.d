;;;
;;; Customizations for lisp like modes.
;;;
;;; This file contains customizations that are appropriate for
;;; any type of lisp mode.

;; I get the following error message when I try to use lispy.
;; error: Need package `iedit-0.97', but only 0.9.9 is available

;;(use-package lispy
;;  :ensure t
;;  :config
;;  (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1))))



;; Figure out how to add this for Clojure, Scheme and Common Lisp
;; as well.
;; There is no common lispish-mode and therefore there is no good
;; hook to put this on.

;; TODO: Is there a general hook for lisp modes?

;; There is a nice replacement for paraedit called lispy that seems interesting.
;; https://github.com/abo-abo/lispy
