;; Spelling configuration file (GNU Emacs)
;; -----------------------------------
;;  Author: Víctor Terrón
;;  Time-stamp: <2013-07-31 10:25:32 vterron>

;; Use aspell instead of ispell. It is is much better at coming up
;; with suggested spelling and, unlike ispell, can also easily check
;; documents in UTF-8 without having to use a special dictionary
;; [http://emacswiki.org/emacs/InteractiveSpell#toc5]
;;
(setq ispell-program-name "aspell")

;; Set Spanish as the default dictionary
(setq ispell-dictionary "castellano")

;; Change the dictionary by cycling through different languages
;; [http://www.emacswiki.org/emacs/FlySpell#toc5]
;;
(let ((langs '("american" "castellano")))
  (setq lang-ring (make-ring (length langs)))
  (dolist (elem langs) (ring-insert lang-ring elem)))

(defun cycle-ispell-languages ()
  (interactive)
  (let ((lang (ring-ref lang-ring -1)))
    (ring-insert lang-ring lang)
    (ispell-change-dictionary lang)))

;; Use auto-dictionary-mode to automatically detect the language of
;; the buffer and choose the dictionary accordingly. The mode, which
;; evaluates the content of the buffer when we stop typing for a while,
;; is never enabled, but we use #'adict-guess-dictionary to write our
;; own two functions that automatically select the ispell dictionary
;; and then check the current region or buffer for spelling errors.
;;
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lib/auto-dictionary"))
(require 'auto-dictionary)

(defun autodict-ispell-region ()
  (interactive)
  (adict-guess-dictionary)
  (ispell-region (region-beginning) (region-end)))

(defun autodict-ispell-buffer ()
  (interactive)
  (adict-guess-dictionary)
  (ispell-buffer))

(provide 'vterron-spell)
