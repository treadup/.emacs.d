;;; setup-wanderlust --- Customization for Wanderlust

;;; Commentary:

;; Wanderlust is an Emacs email client.

;;; Code:

;; Could not install wanderlust with use-package.
(unless (package-installed-p 'wanderlust)
   (package-refresh-contents)
   (package-install 'wanderlust))


;; Wanderlust
(autoload 'wl "wl" "Wanderlust" t)
(autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
(autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)

;; Configure Wanderlust as the default mail composer. This enables you
;; to run Wanderlust using the C-x m key chord.
(autoload 'wl-user-agent-compose "wl-draft" nil t)
(if (boundp 'mail-user-agent)
    (setq mail-user-agent 'wl-user-agent))
(if (fboundp 'define-mail-user-agent)
    (define-mail-user-agent
      'wl-user-agent
      'wl-user-agent-compose
      'wl-draft-send
      'wl-draft-kill
      'mail-send-hook))

(provide 'setup-wanderlust)
;;; setup-wanderlust ends here
