
;;;
;;; Dracula theme
;;;

;; A dark theme for emacs.
;; https://draculatheme.com/emacs/
;; (use-package dracula-theme
;;  :ensure t)
;;
;; This has the problem that the current line highlighting disappears.

;;;
;;; Built in themes
;;;

;; Lets try some of the built in themes.
;; I can't really find one that I like.
;; (load-theme 'deeper-blue)

;; Here are some other good built in themes.
;; deeper-blue
;; misterioso
;; tango-dark
;; tshd-dark
;; wheatgrass

;;;
;;; Tomorrow Night Bright (Clojure True and Brave)
;;;

;; A theme that I have used previously and that is the theme for the Clojure True and Brave emacs.d 
;; is the Tomorrow Night Bright.
;; Unfortunately it does not seem to be available via any package repos.
;; https://github.com/chriskempson/tomorrow-theme/blob/master/GNU%20Emacs/color-theme-tomorrow.el

;;;
;;; Base 16
;;;

;; There is an issue with using the base16-theme in a terminal. I get a weird blue background.
;; See the following discussion of a similar problem.
;; https://github.com/bbatsov/solarized-emacs/issues/18

;; A workaround is to use different themes for the graphical environment and for the terminal.

(if (display-graphic-p)
    (use-package base16-theme
      :ensure t
      ;; The t at the end load the theme without giving any security warnings.
      :config  (load-theme 'base16-solar-flare t))
    (use-package zenburn-theme
      :ensure t
      :config
      (load-theme 'zenburn t)))
;; The main theme I use is a base16-theme that is called base16-solar-flare
;;
;; Some other good choices for themes
;; base16-materia
;; base16-oceanicnext
;; base16-harmonic16-dark
;; base16-solar-flare
;; base16-solarized-dark

;;;
;;; Solarized
;;;

;; Did not like this theme at all.
;; (use-package solarized-theme
;;  :ensure t)
  
;; The following article has some nice code for creating your own custom color theme.
;; http://www.jesshamrick.com/2012/09/18/emacs-as-a-python-ide/
;;
