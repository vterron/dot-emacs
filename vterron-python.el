;; Python configuration file (GNU Emacs)
;; -----------------------------------
;;  Author: Víctor Terrón
;;  Time-stamp: <2012-03-29 17:25:36 vterron>

;; The Python mode created by the Python community
(require 'python-mode)

;; Have all Python (*.py) files to be in python-mode
;; [http://www.emacswiki.org/emacs/AutoModeAlist]
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

;; Enable abbreviation expansion in python-mode
;; [http://emacswiki.org/emacs/AbbrevMode]
(add-hook 'python-mode-hook (lambda () (abbrev-mode 1)))

;; Activate syntax highlighting 
(add-hook 'python-mode-hook 'turn-on-font-lock)

;; Display line numbers in the left margin
(add-hook 'python-mode-hook 'linum-mode)
(setq linum-format "%-4d")

;; Delete all the trailing whitespaces when writing to a file
(add-hook 'write-file-hooks 'delete-trailing-whitespace)

;; There are many, many things that can be tweaked in order to
;; configure Emacs as a superb Python programming environment:
;; http://emacswiki.org/emacs/PythonProgrammingInEmacs
;; http://www.saltycrane.com/blog/2010/05/my-emacs-python-environment/

(provide 'vterron-python)