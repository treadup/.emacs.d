;;; setup-bookmarks --- Bookmarking system

;;; Commentary:

;;; Code:

(defun insert-bookmark-category (category)
  "Insert a bookmark CATEGORY."
  (interactive "sEnter bookmark category: ")
  (move-beginning-of-line 1)
  (insert "    " "<h2>" category "</h2>") (newline)
  (insert "    " "<ul class=\"content-list\">") (newline)
  (insert "    ") (newline)
  (insert "    " "</ul>") (newline)
  (forward-line -2)
  (move-end-of-line 1)
  (insert "    "))

(global-set-key (kbd "C-c c") 'insert-bookmark-category)

(defun insert-bookmark-item (label url)
  "Insert a bookmark for URL labled with LABEL."
  (interactive "sLabel: \nsURL: ")
  (insert "        " "<li><a href=\"" url "\">" label "</a></li>"))

(global-set-key (kbd "C-c u") 'insert-bookmark-item)

(defconst bookmarks-directory "~/code/python/personal-website/website/templates/website/bookmarks")

(defun insert-line (line)
  "Insert LINE followed by a newline."
  (insert line)
  (newline))

(defun find-bookmark-file (category)
  "Create a new bookmark file for CATEGORY if one does not already exist."
  (interactive "sCategory: ")
  (find-file (concat bookmarks-directory "/" category ".html"))
  (if (= (buffer-size (current-buffer)) 0)
    (progn
      (insert-line "{% extends \"website/base.html\" %}")
      (insert-line "")
      (insert-line "{% block content %}")
      (insert-line "<div class=\"content\">")
      (insert-line "")
      (insert-line "</div>")
      (insert-line "{% endblock %}")
      (forward-line -3)
      (insert "    "))
    (message (concat "Bookmark file " category ".html already exists."))))

(global-set-key (kbd "C-c b") 'find-bookmark-file)

(provide 'setup-bookmarks)
;;; setup-ag.el ends here
