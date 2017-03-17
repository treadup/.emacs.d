;;;
;;; Customizations for tailing files.
;;;

;; I want to be able to tail log files using Emacs. It would be good if you can do this using TRAMP
;; to view remote log files.

;; There is a package called itail. The problem with this package is that it uses its own ELPA repo.
;; Another problem is that I cannot get the package to work.

;; Also it would be interesting if you could filter the log using elisp.

;; Some things that you probably want to do when tailing files are the following.
;; 1. Read only buffers
;; 2. No line numbers
;; 3. You might want to disable font locking.

;; There is a mode called auto-revert-tail-mode that can be used to view log files.
;; However this mode seems really slow.
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Reverting.html

;; Here is a post describing how to set this up using plain auto revert tail mode.
;; http://emacs.stackexchange.com/questions/13005/is-there-a-decent-log-viewing-mode-for-large-log-files

(use-package logview
  :ensure t)

;; automagically tail log files
;; (add-to-list 'auto-mode-alist '("\\log.txt\\'" . logview-modeauto-revert-tail-mode))

;;(defun pht-log-tail-handler ()
;;  (end-of-buffer)
;;  (make-variable-buffer-local 'auto-revert-interval)
;;  (setq auto-revert-interval 1)
;;  (auto-revert-set-timer)
;;  (make-variable-buffer-local 'auto-revert-verbose)
;;  (setq auto-revert-verbose nil)
;;  (read-only-mode t)
;;  (font-lock-mode 0)
;;  (linum-mode 0))

;; (add-hook 'auto-revert-tail-mode-hook 'pht-log-tail-handler)

