;; A dark theme for emacs.
;; https://draculatheme.com/emacs/
;; (use-package dracula-theme
;;  :ensure t)
;;
;; This has the problem that the current line highlighting disappears.

;; Lets try some of the built in themes.
;; I can't really find one that I like.
;; (load-theme 'deeper-blue)

;; Here are some other good built in themes.
;; deeper-blue
;; misterioso
;; tango-dark
;; tshd-dark
;; wheatgrass

;; A theme that I have used previously and that is the theme for the Clojure True and Brave emacs.d 
;; is the Tomorrow Night Bright.
;; Unfortunately it does not seem to be available via any package repos.
;; https://github.com/chriskempson/tomorrow-theme/blob/master/GNU%20Emacs/color-theme-tomorrow.el

(use-package base16-theme
  :ensure t
  :config
  (load-theme 'base16-solar-flare t)) ;; The t at the end load the them without giving any security warnings.

;; Some other good choices for themes
;; base16-materia
;; base16-oceanicnext
;; base16-harmonic16-dark
;; base16-solar-flare
;; base16-solarized-dark
