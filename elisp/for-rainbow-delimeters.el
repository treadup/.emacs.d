;;;
;;; Customizations for rainbow delimiters
;;;

;; Rainbow colored delimiters. Extremely useful if you are programming
;; in a lisp like language.
;; https://www.emacswiki.org/emacs/RainbowDelimiters
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))
