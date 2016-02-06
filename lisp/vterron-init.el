;; GNU Emacs top-level configuration file
;; --------------------------------------
;;  Author: Víctor Terrón
;;  Time-stamp: <2012-03-29 17:25:48 vterron>

(defun author-name  () "Víctor Terrón")
(defun author-email () "vterron@iaa.es")

(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lib"))

(require 'vterron-abbrev)
(require 'vterron-backups)
(require 'vterron-generic)
(require 'vterron-keys)
(require 'vterron-mutt)
(require 'vterron-python)
(require 'vterron-spell)
(require 'vterron-theme)

;; Supress the startup echo area message. This *must* be here (.emacs)
;; Please note that Emacs forces us to hardcode our login name here
(setq inhibit-startup-echo-area-message "vterron")

;; Let us know how much time Emacs is taking to load.
;; [http://cheat.errtheblog.com/s/emacs_tips/]
;;
(message "Emacs loaded in %fs"
  (* 0.000001 (apply #'-
    (mapcar (lambda (time)
              (+ (* 1000000 (+ (* 65536 (first time))
              (second time))) (third time)))
            (list (current-time) before-init-time)))))
