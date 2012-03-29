;; Backup and auto-save configuration file (GNU Emacs)
;; -----------------------------------
;;  Author: Víctor Terrón
;;  Time-stamp: <2012-03-29 16:35:35 vterron>

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

(provide 'vterron-backups)