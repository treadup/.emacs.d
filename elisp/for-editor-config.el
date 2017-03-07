;;;
;;; Customization for EditorConfig
;;;

;; Editor config is a standard for being able to specify per project
;; specific formatting. This is useful if the project you are working
;; on uses a different indentation convention from what you normally use.

;; www.editorconfig.org
;; https://github.com/editorconfig/editorconfig-emacs

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))


