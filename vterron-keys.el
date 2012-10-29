;; Key bindings configuration file (GNU Emacs)
;; -----------------------------------
;;  Author: Víctor Terrón
;;  Time-stamp: <2012-10-29 18:03:31 vterron>

;; Prevent Emacs from being sent to background if we accidentally
;; press Ctrl+Z [http://www.fettesps.com/emacs-disable-suspend-button/]
(global-unset-key [(control z)])
(global-unset-key [(control x)(control z)])

;; Allow left/right arrow key navigation of the buffer list and deactivate
;; up/down in iswitchb [http://emacswiki.org/emacs/IswitchBuffers]
;;
(defun iswitchb-local-keys ()
  (mapc (lambda (K)
	  (let* ((key (car K)) (fun (cdr K)))
	    (define-key iswitchb-mode-map (edmacro-parse-keys key) fun)))
	'(("<right>" . iswitchb-next-match)
	  ("<left>"  . iswitchb-prev-match)
	  ("<up>"    . ignore             )
	  ("<down>"  . ignore             ))))
(add-hook 'iswitchb-define-mode-map-hook 'iswitchb-local-keys)

;; Execute a region in a Python interpreter
(eval-after-load "python-mode"
  '(progn
     (add-hook 'python-mode-hook
	       (lambda ()
		 (local-set-key (kbd "C-c C-e") 'py-execute-region)))))

(global-set-key [f4] 'ispell-region)
(global-set-key [f5] 'ispell-buffer)
;; Cycle through languages (function defined in vterron-spell.el)
(global-set-key [f6] 'cycle-ispell-languages)

;; F5 is not working over SSH (instead, a tilde is inserted), so we
;; also have to bind what Emacs is actually receiving (C-q <F5>) to
;; the same function. This is not needed, strangely enough, for F6.
;;
(global-set-key "[15~" 'ispell-buffer)

;; Bind Refill mode to F8. Although most of the time we will want
;; perfectly filled paragraphs in our buffers, it must be temporarily
;; disabled if we need to write a table or anything else non-filled.
;;
(global-set-key [f8] 'refill-mode)

;; The various ways of sanely setting "unusual" keybindings (like
;; M-left and C-Home and such) tend not to work for terminal Emacs.
;; We need to actually figure out what string of characters the
;; terminal emulator is sending to Emacs and bind the function to
;; those. This function returns the right value (the terminal
;; equivalent) for the string representation of a keystroke.
;; [http://nex-3.com/posts/45-efficient-window-switching-in-emacs]
;;
(defvar real-keyboard-keys
  '(("M-<up>"        . "\M-[1;3A")
    ("M-<down>"      . "\M-[1;3B")
    ("M-<right>"     . "\M-[1;3C")
    ("M-<left>"      . "\M-[1;3D")
    ("C-<return>"    . "\C-j")
    ("C-<delete>"    . "\M-[3;5~")
    ("C-<up>"        . "\M-[1;5A")
    ("C-<down>"      . "\M-[1;5B")
    ("C-<right>"     . "\M-[1;5C")
    ("C-<left>"      . "\M-[1;5D")))

(defun key-literal (desc)
  (or (and window-system (read-kbd-macro desc))
      (or (cdr (assoc desc real-keyboard-keys))
          (read-kbd-macro desc))))

;; Move between windows using Alt and the arrow keys.
;; [http://emacswiki.org/emacs/WindMove]
(global-set-key (key-literal "M-<left>")  'windmove-left)
(global-set-key (key-literal "M-<right>") 'windmove-right)
(global-set-key (key-literal "M-<up>")    'windmove-up)
(global-set-key (key-literal "M-<down>")  'windmove-down)

;; And switch between buffers with Control and the arrow keys.
(require 'prev-next-buffer)
(global-set-key (key-literal "C-<left>")  'previous-buffer)
(global-set-key (key-literal "C-<right>") 'next-buffer)

;; Take a look at this for info on how to make Emacs switch before two
;; recently-used buffers. Ideally, the buffers we switch between most
;; would be near to each other (minimizing the time spent switching)
;; http://emacswiki.org/emacs/SwitchingBuffers

(provide 'vterron-keys)