;;;
;;; Customizations for web mode
;;; 

;; The web mode website is located at the following url.
;; http://web-mode.org/

(defun personal-configure-web-mode () 
  (progn
    (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))))

(use-package web-mode
  :ensure t
  :config (personal-configure-web-mode))
