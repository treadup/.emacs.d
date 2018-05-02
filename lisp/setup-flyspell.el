;;; setup-flyspell --- Customizations for flyspell

;;; Commentary:

;; For now just use flyspell for text mode and markdown mode.
;; https://www.emacswiki.org/emacs/FlySpell

;;; Code:

;; Use flyspell for spellchcking text mode buffers.
(add-hook 'text-mode-hook (lambda () (flyspell-mode 1)))

;; Use flyspell for spellchecking comments in prog mode buffers.
(add-hook 'prog-mode-hook (lambda ()
                           (flyspell-prog-mode)))

(provide 'setup-flyspell)
;;; setup-flyspell ends here
