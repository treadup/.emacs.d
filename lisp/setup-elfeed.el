;;; setup-elfeed --- Customizations for elfeed

;;; Commentary:

;; https://github.com/skeeto/elfeed

;;; Code:
(use-package elfeed
  :ensure t
  :config
  (global-set-key (kbd "C-c w") 'elfeed)

  (setq elfeed-feeds
    '("https://news.ycombinator.com/rss"
       "https://boingboing.net/feed"
       "http://planet.emacsen.org/atom.xml")))

(provide 'setup-elfeed)
;;; setup-elfeed.el ends here
