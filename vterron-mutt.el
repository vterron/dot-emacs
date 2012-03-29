;; Mutt configuration file (GNU Emacs)
;; -----------------------------------
;;  Author: Víctor Terrón
;;  Time-stamp: <2012-03-29 09:30:32 vterron>

;; Have all files whose name contains "/mutt" to be in mail-mode.
;; [http://www.emacswiki.org/emacs/MuttInEmacs]
(add-to-list 'auto-mode-alist '("/mutt" . mail-mode))

;; Enable abbreviation expansion in mail-mode.
;; [http://emacswiki.org/emacs/AbbrevMode]
(add-hook 'mail-mode-hook (lambda () (abbrev-mode 1)))

;; Do automatic refilling, all the time. Basically, this hits M-q
;; automatically after changes to the buffer that might normally
;; trigger auto-filling [http://www.emacswiki.org/emacs/RefillMode]
;;
(add-hook 'mail-mode-hook 'refill-mode)

;; Use hard newlines; otherwise Emacs will move the cursor back to the
;; last paragraph when I try to insert my mail signature at the end of
;; the message. [http://www.emacswiki.org/emacs/ManiacMode]
;;
(add-hook 'mail-mode-hook (lambda () (use-hard-newlines 1 t)))

;; The maximum line width for filling paragraphs in mail-mode.
;; The 72-char limit basically allows for bodies to then be quoted,
;; so it gives room for the additional chars added to a line.
;; [http://www.ietf.org/rfc/rfc2822.txt]
;;
(add-hook 'mail-mode-hook (lambda () (setq fill-column 72)))

(provide 'vterron-mutt)