;;; setup-elfeed --- Customizations for elfeed

;;; Commentary:

;; https://github.com/skeeto/elfeed

;;; Code:
(use-package elfeed
  :ensure t
  :config
  (setq elfeed-feeds
    '("https://news.ycombinator.com/rss"
       "https://boingboing.net/feed"
       "http://planet.emacsen.org/atom.xml")))

(provide 'setup-elfeed)
;;; setup-elfeed.el ends here
