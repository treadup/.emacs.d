;;;
;;; Customization for flycheck.el
;;;

;; Flycheck provides modern on the fly syntax checking.
;; http://www.flycheck.org/en/latest/

(use-package flycheck
  :ensure t
  :diminish "FC")

(global-flycheck-mode)
