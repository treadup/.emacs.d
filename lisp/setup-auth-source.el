;;;
;;; Customizations for auth source.
;;;

;; The standard for storing auth information in Unix seems to be
;; on of the following files in the home folder.
;; .netrc
;; .authsource
;; .authsource.gpg

;; If you want the .authsource.gpg file to be encrypted for a specific
;; recipient without having to specify a key you can use a file local
;; variable to specify which key should be used.
;;
;; The contents of the .authsource.gpg file should be something like
;; the following.
;;
;;    # -*- epa-file-encrypt-to: ("henrik@peacefulrainforest.org") -*-
;;    machine northwood login henrik password secret
;;
;; Note that automatic encryption will not work in fundamental mode.
;; But it you switch to shell mode and then save the file automatic
;; encryption will work.
;;

;; Setting auth-source-debug to true will enable debugging information.
;; (setq auth-source-debug t)
