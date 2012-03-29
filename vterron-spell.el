;; Spelling configuration file (GNU Emacs)
;; -----------------------------------
;;  Author: Víctor Terrón
;;  Time-stamp: <2012-03-29 16:32:55 vterron>

;; Use aspell instead of ispell. It is is much better at coming up
;; with suggested spelling and, unlike ispell, can also easily check
;; documents in UTF-8 without having to use a special dictionary
;; [http://emacswiki.org/emacs/InteractiveSpell#toc5]
;;
(setq ispell-program-name "aspell")

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

;; Take a look at this for info on how to automatically detect the
;; language and choose a dictionary accordingly (GuessBufferLanguage)
;; http://emacswiki.org/emacs/InteractiveSpell#toc3
;; http://www.emacswiki.org/emacs/GuessLang
;;
;; Also, I would like ispell to ignore quoted text when in mail-mode.
;; A hurried and cursory Google search did not result anything.

(provide 'vterron-spell)