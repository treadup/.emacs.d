;;; setup-ui --- Customizations for User Interface

;;; Commentary:

;;; Code:

;;
;; User Interface
;;

;; Hide startup screen
(setq inhibit-startup-screen t)

;; Do not show any window decorations.
;; This only works on Emacs 26 or later.
;; (set-frame-parameter nil 'undecorated t)

;; Show line numbers

;; The display-line-numbers-mode was added in Emacs 26.1
;; It is a replacement for linum-mode.
(if (boundp 'display-line-numbers)
  (progn
    (add-hook 'prog-mode-hook 'display-line-numbers-mode)
    (add-hook 'text-mode-hook 'display-line-numbers-mode))
  (progn
    (add-hook 'prog-mode-hook 'linum-mode)
    (add-hook 'text-mode-hook 'linum-mode)))

(defun system-specific-default-font-family ()
  "Determines the default font family to use."
  (if (string-equal system-type "darwin")
    "Source Code Pro"
    "Fira Code"))

;; Set default font used by all frames.
(set-face-attribute
   'default nil
   :family (system-specific-default-font-family)
   :height 180
   :weight 'normal
   :width 'normal)

;; Here are some possible font families that you can use.
;; "Fira Code"
;; "Inconsolata"
;; "DejaVu Sans Mono"

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
(setq initial-frame-alist '((top . 0) (left . 0) (width . 129) (height . 34)))

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

;;; When on macOS use dark appearance and have a transparent titlebar.
(if (string-equal system-type "darwin")
  (progn
    (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
    (add-to-list 'default-frame-alist '(ns-appearance . dark))))

;; This variable is deprecated as of Emacs 24.1
;; (setq tooltip-use-echo-area t)

;; Show menu bar if we are in graphics mode
;; (if (display-graphic-p)
;;    (menu-bar-mode t))

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

;; In terminal mode use the same color for the foreground and
;; background color of the vertical border.
;; (set-face-background 'vertical-border "gray")
;; (set-face-foreground 'vertical-border (face-background 'vertical-border))

(provide 'setup-ui)
;;; setup-ui.el ends here
