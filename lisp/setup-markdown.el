;;; setup-markdown --- Customization for markdown

;;; Commentary:

;; The markdown-mode package provides two major-modes
;; 1. markdown-mode
;; 2. gfm-mode
;;
;; markdown-mode is a major mode for standard markdown.
;; gfm-mode is a major mode for GitHub flavored markdown.

;;; Code:

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode)))

(provide 'setup-markdown)
;;; setup-markdown ends here
