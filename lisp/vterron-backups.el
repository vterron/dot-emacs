;; Backup and auto-save configuration file (GNU Emacs)
;; -----------------------------------
;;  Author: Víctor Terrón
;;  Time-stamp: <2013-07-18 21:52:43 vterron>

;; Emacs creates a backup only when we save the first time. This is
;; incompatible with the behavior of leaving the buffers open all the
;; time and constantly making changes and writing them to disk. We
;; want to have Emacs backup each time we save the buffer.
;; [http://www.emacswiki.org/emacs/BackupEachSave]
;;
(require 'backup-each-save)
(add-hook 'after-save-hook 'backup-each-save)

;; Replace the default backup directory tree ("~/.backups") used by
;; backup-each-save. Every file will be saved here with a timestamp
;; suffix to make multiple saves of the same file unique.
;;
(setq backup-each-save-mirror-location "~/.emacs-backups/")

;; Do not put auto-save files scattered all over the file system
;; [http://cheat.errtheblog.com/s/emacs_tips/]
;;
(defvar autosave-directory "~/.emacs-autosaves/")
(make-directory autosave-directory t)
(setq auto-save-file-name-transforms
  `((".*" ,autosave-directory t)))

;; Path to the directory where information about interrupted sessions
;; (in files named .saves-pid-hostname) is saved for later recovery.
;; [http://www.gnu.org/software/emacs/manual/html_node/emacs/Recover.html]
;;
(defvar autosave-list-directory
  (concat autosave-directory "auto-save-list/"))
(setq auto-save-list-file-prefix autosave-list-directory)

;; Count the number of lines in the buffer "*async delete-backups*", each
;; one of them corresponding to a file removed by delete-old-backups, and
;; display in the echo area a message like "Deleted 117 old backup files"
;; [Modified from: http://stackoverflow.com/a/16134238/184363]
;;
(defun delete-old-backups-sentinel (process event)
  (cond ((string-match-p "finished" event)
	 (save-excursion
	   (let (output-buffer words number-of-lines)
	     (setq output-buffer "*async delete-backups*")
	     (set-buffer output-buffer)
	     ;; Write something like "Page has 117 lines (117 + 0)" to the
	     ;; echo area, then extract from there the number of lines (in
	     ;; this example it would be 117, the third word)
	     (with-temp-message ""
		(count-lines-page)
	        (setq words (split-string (current-message)))
	        (setq number-of-lines (string-to-number (elt words 2))))
	     (when (/= number-of-lines  0)
	       (message "Deleted %d old backup files" number-of-lines))
	     (kill-buffer output-buffer))))))

;; Delete those regular files in ~/.emacs-backups that were last modified
;; six months ago. The command is run asynchronously. Upon completion, if
;; at least one file has been deleted, a message is displayed in the echo
;; area letting us know the number.
;;
(defun delete-old-backups ()
  "Remove files in ~/.emacs-backups older than six months"
  (interactive)
  (let ((process (start-process-shell-command
		  "delete-backups"
		  "*async delete-backups*"
		  (concat "find ~/.emacs-backups/ -type f -mtime +180 -print0 | "
			  "xargs --null rm --verbose --force")
		  )))
    (set-process-sentinel process 'delete-old-backups-sentinel)))

(provide 'vterron-backups)
