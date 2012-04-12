;; General configuration file (GNU Emacs)
;; ----------------------------
;;  Author: Víctor Terrón
;;  Time-stamp: <2012-04-12 13:10:49 vterron>

;; Override Emacs' default mechanism for buffer names with a more sensible
;; behaviour which uses parts of the file names to make the buffer names.
;; distinguishable: http://emacswiki.org/emacs/uniquify
;;
(require 'uniquify)

;; Keep custom variables in their own file
(setq custom-file "~/.emacs-custom.el")
(load custom-file 'noerror)

;; iswitchb-buffer, an alternative to switch-to-buffer. As we type the buffer
;; name, the list of options is reduced to the buffers matching what we typed.
;; And it is not limited to the beginning of the buffer name, either.
;;
(iswitchb-mode)

; Prevent certain buffers from showing up in the completion list
(add-to-list 'iswitchb-buffer-ignore "*Messages*")

;; Rename an open document: like "Save As", but also refreshes the buffer
;; with the new file name: [http://www.stringify.com/2006/apr/24/rename/]
;;
(defun rename-current-file-or-buffer ()
  (interactive)
  (if (not (buffer-file-name))
      (call-interactively 'rename-buffer)
    (let ((file (buffer-file-name)))
      (with-temp-buffer
        (set-buffer (dired-noselect file))
        (dired-do-rename)
        (kill-buffer nil))))
  nil)

;; Reload the .emacs file without restarting Emacs. Note that this strategy
;; does not guarantee to give a totally revised configuration, as previously
;; loaded definitions are not removed. For that, Emacs must be restarted afresh.
;; [http://www.saltycrane.com/blog/2007/07/how-to-reload-your-emacs-file-while/]
;;
(defun reload-dotemacs ()
  (interactive)
  (load-file "~/.emacs"))

;; Display current line and column numbers
(setq line-number-mode  't)
(setq column-number-mode t)

;; Don't show the startup screen
(setq inhibit-startup-screen t)

;; The content of the scratch buffer when Emacs starts up
(setq initial-scratch-message
  ";; And God said, Let there be light: and there was light.\n\n")

;; Indent the whole current buffer, replace tabs with spaces and remove
;; trailing spaces [http://emacsblog.org/2007/01/17/indent-whole-buffer/]
;;
(defun indent-buffer ()
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

;; Keyboard scroll one line at a time, even when the cursor is moved past the
;; top or bottom of the window [http://www.emacswiki.org/emacs/SmoothScrolling]
;;
(setq scroll-step 1)
(setq scroll-conservatively 10000)

;; Scroll one line up or down without moving the cursor
;; [http://geosoft.no/development/emacs.html#Scroller]
;;
(defun scroll-down-keep-cursor ()
  (interactive)
  (scroll-down 1))

(defun scroll-up-keep-cursor ()
  (interactive)
  (scroll-up 1))

;; Put the scrollbar on the right
(setq scroll-bar-mode-explicit t)
(set-scroll-bar-mode 'right)

;; Run a shell in the bottom window. The Emacs terminal emulator imitates the
;; VT100-style ANSI escape codes, just like xterm or rxvt. It isn't complete or
;; perfect, so some interactive programs won't work properly, but most things
;; like top and ssh will. No more switching back and forth in GNU Screen!
;; [http://www.masteringemacs.org/articles/2010/11/01/running-shells-in-emacs-overview/]
;;
(defun small-shell ()
  (interactive)
  (split-window-vertically)
  (other-window 1)
  (shrink-window (- (window-height) 12))
  (ansi-term))

;; Update time stamps automatically
;; http://www.emacswiki.org/emacs/TimeStamp
(setq time-stamp-pattern nil)
(add-hook 'before-save-hook 'time-stamp)

;; Enable some of the advanced features disabled by default
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)

;; Disable the tool and menu bars completely
;; [http://emacswiki.org/emacs/ToolBar]
(menu-bar-mode -1)
(tool-bar-mode -1)

;; Yank killed or copied at point instead of mouse position. In this
;; manner it does not matter precisely where we click, as long as we
;; click on the window.
(setq mouse-yank-at-point t)

(provide 'vterron-generic)
