;;;
;;; Customizations for User Interface
;;;
;;; This file contains user interface specific things.
;;;

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
