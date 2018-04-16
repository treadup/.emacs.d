;;; setup-ui --- Customizations for User Interface

;;; Commentary:

;;; Code:

;;
;; User Interface
;;

;; Hide startup screen
(setq inhibit-startup-screen t)

;; Show line numbers
(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'text-mode-hook 'linum-mode)

(if (not (display-graphic-p))
    (setq linum-format "%d "))

;; Show menu bar if we are in graphics mode
;; TOOD: Probably disable this unless we are on a Mac
(if (display-graphic-p)
    (menu-bar-mode t))

;; Set default font
;; Increase font size.
;; Play with the :height attribute to change size
(set-face-attribute 'default nil
                    :family "Source Code Pro"
                    :height 160
                    :weight 'normal
                    :width 'normal)

;; No cursor blinking
(blink-cursor-mode 0)

;; Title bar path
;; Full path in the title bar
(setq-default frame-title-format "%b (%f)")

;; Set the width and height of the emacs window in characters.
;; (setq initial-frame-alist '((top . 0) (left . 0) (width . 130) (height . 40)))
;; This maximizes the frame on my MacBook.
;;
;; Perhaps have some way of detecting which screen I am on and modifying the size
;; of the frame dynamically at startup.
(setq initial-frame-alist '((top . 0) (left . 0) (width . 126) (height . 36)))

;; Turn of the visual bell.
;;
;; On OS X this disables the yellow warning triangle.
;; https://www.emacswiki.org/emacs/AlarmBell
(setq visible-bell nil)

;; Disable audio bell
(setq ring-bell-function 'ignore)

;; Show tabs with a special glyph that kind of looks like >> but smaller.
;; Here is a 	tab character so you can see how it looks.
;;(setq whitespace-style (quote (spaces tabs newline space-mark tab-mark newline-mark)))
;; (setq whitespace-style (quote (tabs tab-mark)))
;; (global-whitespace-mode 1)

;; Display tooltips in the echo area.
(tooltip-mode -1)

;; This variable is deprecated as of Emacs 24.1
;; (setq tooltip-use-echo-area t)

;; Remove menu bar
(menu-bar-mode -1)

;; Remove toolbar
(tool-bar-mode -1)

;; Remove scrollbars
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)

;; When you visit a file, point goes to the last place where it
;; was when you previously visited the same file.
;; http://www.emacswiki.org/emacs/SavePlace
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (concat user-emacs-directory "places"))

;; Highlights matching parenthesis
(show-paren-mode 1)

;; Highlight current line
(global-hl-line-mode 1)

;; Interactive search key bindings. By default, C-s runs
;; isearch-forward, so this swaps the bindings.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;;
;; Clipboard interaction
;;

;; In general this area is a mess. See https://www.emacswiki.org/emacs/CopyAndPaste
;; for more information.

;; Use the clipboard when killing and yanking
(setq select-enable-clipboard t)

;; Save clipboard strings into kill ring before replacing them.
;; When one selects something in another program to paste it into Emacs,
;; but kills something in Emacs before actually pasting it,
;; this selection is gone unless this variable is non-nil
(setq save-interprogram-paste-before-kill t)

(provide 'setup-ui)
;;; setup-ui.el ends here
