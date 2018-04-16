;;; setup-clojure --- Customizations for Clojure

;;; Commentary:
;;; This file contains things that are language specific to Clojure.

;;; Code:

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

;;
;; Cider Keybindings
;;

(defun cider-start-http-server ()
  (interactive)
  (cider-load-current-buffer)
  (let ((ns (cider-current-ns)))
    (cider-repl-set-ns ns)
    (cider-interactive-eval (format "(println '(def server (%s/start))) (println 'server)" ns))
    (cider-interactive-eval (format "(def server (%s/start)) (println server)" ns))))

(defun cider-refresh ()
   (interactive)
   (cider-interactive-eval (format "(user/reset)")))

(defun cider-user-ns ()
   (interactive)
   (cider-repl-set-ns "user"))


(defun extra-keybindings-for-cider ()
    (define-key clojure-mode-map (kbd "C-c C-v") 'cider-start-http-server)
    (define-key clojure-mode-map (kbd "C-M-r") 'cider-refresh)
    (define-key clojure-mode-map (kbd "C-c u") 'cider-user-ns)
    (define-key cider-mode-map (kbd "C-c u") 'cider-user-ns))

;; integration with a Clojure REPL
;; https://github.com/clojure-emacs/cider


;;
;; Cider
;;
(defun config-for-cider ()

  ;; provides minibuffer documentation for the code you're typing into the repl
  ;; Change this to
  ;; https://groups.google.com/forum/#!topic/clojure/suo83_S3Luo
  ;; https://github.com/syl20bnr/spacemacs/pull/5828
  ;; (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
  (add-hook 'cider-mode-hook 'eldoc-mode)

  ;; go right to the REPL buffer when it's finished connecting
  (setq cider-repl-pop-to-buffer-on-connect t)

  ;; When there's a cider error, show its buffer and switch to it
  (setq cider-show-error-buffer t)
  (setq cider-auto-select-error-buffer t)

  ;; Where to store the cider history.
  (setq cider-repl-history-file "~/.emacs.d/cider-history")

  ;; Wrap when navigating history.
  (setq cider-repl-wrap-history t)

  (extra-keybindings-for-cider))

;;
;; Cider package
;;

(use-package cider
  :ensure t
  :config
  (config-for-cider))

(provide 'setup-clojure)
;;; setup-clojure.el ends here
