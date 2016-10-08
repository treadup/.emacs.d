;;
;; Customizations for Text
;;

;; This file contains customizations for editing text documents.
;;

;; Open markdown files in text mode.
(add-to-list 'auto-mode-alist '("\\.md\\'" . text-mode))

;; In general you can fill a paragraph by pressing the M-q
;; key combination. This will reflow the text. 

;; Turn on autofill-mode for text modes.
(add-hook 'text-mode-hook 'turn-on-auto-fill)
