;; Abbreviations configuration file (GNU Emacs)
;; -----------------------------------
;;  Author: Víctor Terrón
;;  Time-stamp: <2012-03-28 14:53:07 vterron>

;; Tell emacs where to read abbreviations definitions from
;; [http://emacswiki.org/emacs/AbbrevMode]
(setq abbrev-file-name "~/.emacs.d/abbrev_defs")

;; Read the abbreviations on startup, avoid errors if it's missing
(if (file-exists-p abbrev-file-name)
  (quietly-read-abbrev-file))

;; Save abbreviations whenever we save files or quit Emacs
(setq save-abbrevs t)

;; Take a look at this for info on how replace the abbreviation
;; *and* repositions the cursor (short answer: SkeletonMode)
;; http://emacswiki.org/emacs/AbbrevMode#toc7
;; http://www.emacswiki.org/emacs/SkeletonMode#toc5

(provide 'vterron-abbrev)