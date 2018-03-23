;;;
;;; Customizations for OS X
;;;

;;
;; Cut and Paste
;;

;; Fixes cut and paste for OS X.
;; https://www.emacswiki.org/emacs/Comments_on_CopyAndPaste

(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)
