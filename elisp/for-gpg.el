;;;
;;; Customizations for EasyPG
;;;

;; Configure emacs to allow pinentry for GPG in the minibuffer.
;; For this to work you have to have allow-emacs-pinentry in
;; the gpg-agent.conf file.
;; https://elpa.gnu.org/packages/pinentry.html

(use-package pinentry
  :ensure t
  :config
  (pinentry-start))
