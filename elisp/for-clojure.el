;;;
;;; Customizations for Clojure
;;;
;;; This file contains things that are language specific to Clojure.
;;;

;;
;; Clojure
;;

;; key bindings and code colorization for Clojure
;; https://github.com/clojure-emacs/clojure-mode
(use-package clojure-mode
  :ensure t
  :config
  (progn
    ;; This is useful for working with camel-case tokens, like names of
    ;; Java classes (e.g. JavaClassName)
    (add-hook 'clojure-mode-hook 'subword-mode)
    ;; syntax hilighting for midje
    (add-hook 'clojure-mode-hook
        (lambda ()
            (setq inferior-lisp-program "lein repl")
            (font-lock-add-keywords
             nil
             '(("(\\(facts?\\)"
                (1 font-lock-keyword-face))
               ("(\\(background?\\)"
                (1 font-lock-keyword-face))))
            (define-clojure-indent (fact 1))
            (define-clojure-indent (facts 1))))))

;; extra syntax highlighting for clojure
(use-package clojure-mode-extra-font-locking
  :ensure t)


