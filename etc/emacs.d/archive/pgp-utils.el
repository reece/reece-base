;;
;; $RCSfile: pgp-utils.el,v $
;; $Revision: 0.5 $
;; $Date: 1996/01/17 05:20:54 $
;;
;; pgp-utils -- interface to pgp for signing, encrypting, decrypting, and
;;              signature verfication
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; LICENSE
;;
;; Copyright 1995, Reece Kimball Hart (reece@dasher.wustl.edu)
;;
;; THIS SOURCE IS SUBJECT TO THE TERMS OF THE GNU GENERAL PUBLIC LICENSE.
;; You may obtain this license from prep.ai.mit.edu or a mirror site.  It
;; is also available within emacs as C-h C-c.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; AVAILABILITY
;;
;; LCD Archive Entry:
;; pgp-utils|Reece Kimball Hart|reece@dasher.wustl.edu|
;; pgp interface for all modes|
;; $Date: 1996/01/17 05:20:54 $|$Revision: 0.5 $||
;;
;; ftp://archive.cis.ohio-state.edu/pub/gnu/emacs/elisp-archive/
;;
;; http://dasher.wustl.edu/~reece/emacs/pgp-utils.el
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; AUTHOR
;;
;; Reece Kimball Hart                 |Internet: reece@dasher.wustl.edu
;; Biophysics & Biochemistry, Box 8231|WWW URL:  http://dasher.wustl.edu/~reece
;; Washington Univ. School of Medicine|Phone:    (314) 362-4198 (lab)
;; 660 South Euclid                   |                   -7183 (fax)
;; St. Louis, Missouri  63110    (USA)|PGP public key available by finger & WWW
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; DESCRIPTION
;;
;; INSTALLATION & USAGE
;;
;; REQUIREMENTS
;;
;; ACKNOWLEDGEMENTS
;; pgp-get-password was adapted from Gray Watson's pgp code (pgp.el, I think)
;;
;; EXAMPLE
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODIFICATION HISTORY
;;
;; $Log: pgp-utils.el,v $
;; Revision 0.5  1996/01/17 05:20:54  reece
;; *** empty log message ***
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; ? shell-command return status
; ? buffer v. region commands
; ? password timeout

(defvar pgp-secure-directory
  (expand-file-name "~/lib/pgp")
  "name for directory into which temporary files may be written securely")
(defvar pgp-executable-path
  "pgp"
  "name of pgp program.  May be full path, or just the the name if pgp is along the PATH.")
(defvar pgp-user-name
  (getenv "USER")
  "this variable is used as the name to sign documents")
(defvar pgp-encrypt-for-sender
  nil
  "always, never, ask; t means always include sender on list of encryption recipients; nil means dont (but may be added explicitely)")
(defvar pgp-sign
  "always"
  "always, never, ask")
(defvar pgp-overwrite-region
  "always"
  "always, ask")
(defvar pgp-hide-log
  nil
  "t,nil")
(defvar pgp-remember-password
  nil
  "t,nil")
(defvar pgp-always-ask-name
  t
  "t: always ask for user name when signing documents;
nil: don't ask (use pgp-user-name if set)")
(defvar pgp-utils-version
  (let ((rev "$Revision: 0.5 $"))
	(string-match "[0-9.]*" rev 11)			; rcs version begins at char 11+1
	(substring rev 11 (match-end 0))
	)
  "version number of pgp-utils package"
  )

(defun make-list-from-lines (buffer)
  "Generates a list in which each element is a line from the specified buffer.
This is used by pgp-pub-key-name-list to get a list of the names in the public
keyring."
  (save-excursion
	(set-buffer buffer)
	(goto-char (point-min))
	(let ((linestart (point))
		  (thelist))
	  (while (not (= (point) (point-max)))
		(setq linestart (point))
		(end-of-line)
		(setq thelist
			  (append thelist (list (buffer-substring linestart (point)))))
		(forward-line))
	  thelist
	  )
	)
  )
(defun pgp-command (user-name password recipient-list)
  "generates the appropriate pgp command to sign, encrypt, or decrypt
   pgp messages"
  (if (and user-name (not password))
	  (error "pgp-command: must provide password to decrypt messages"))
  (concat (if (and user-name password)
			  (concat "PGPPASS='" password "'; export PGPPASS; "))
		  pgp-executable-path " -af"
		  (if user-name (concat " -su '" user-name "'"))
		  (if recipient-list (concat " -e " recipient-list))
		  )
  )
(defun pgp-command-on-region (start end pgp-command)
  "pgp-command-on-region(start end pgp-command)
Executes the specified pgp-command on the region bound by start and
end, displays stderr output from the command, and prompts the user to
replace the region with the pgp output."
  (let* ((log-fn (tmpfn "pgp" t))
		 (log-buf-name "*pgp status*")
		 (command (concat pgp-command " 2>" log-fn))
		 )
	(message (concat "Executing pgp command..." log-fn))
	(shell-command-on-region start end command t)
	(display-buffer (get-buffer-create log-buf-name))
	(set-buffer log-buf-name)
	(buffer-disable-undo log-buf-name)
	(setq buffer-read-only nil)
	(erase-buffer)
	(insert-file-contents log-fn)
	(end-of-buffer)
	(insert "====== PGP execution completed ======\n")
	(setq buffer-read-only t)
;	(delete-file log-fn)
	)
  )
(defun pgp-decode-region (start end)
  (interactive "r")
  (let* ((user	(if (y-or-n-p (format "sign document as %s? " pgp-user-name))
					pgp-user-name nil))
		 (pass (if user (pgp-get-password
						 (format "PGP password for %s: " user)) nil))
		 (recipients (pgp-get-recipients))
		 (log-file-name (tmpfn "pgp" t))
		 (log-buf-name "* pgp status *")
		 (command (if (or user recipients)
					  (pgp-command user pass recipients)
					nil))
		 )
	(if command
		(pgp-command-on-region start end command)
	  )
	)
  )
(defun pgp-encode-region (start end)
  (interactive "r")
  (let* ((user	(if (y-or-n-p (format "sign document as %s? " pgp-user-name))
					pgp-user-name nil))
		 (pass (if user (pgp-get-password
						 (format "PGP password for %s: " user)) nil))
		 (recipients (pgp-get-recipients))
		 (log-file-name (tmpfn "pgp" t))
		 (log-buf-name "* pgp status *")
		 (command (if (or user recipients)
					  (pgp-command user pass recipients)
					nil))
		 )
	(if command
		(pgp-command-on-region start end command)
	  )
	)
  )
(defun pgp-get-password (prompt)
  "Get password without echo.  Backspace, delete, C-u line kill, and
C-g aborting are supported.  Adapted from Gray Watson's pgp code.
reece@dasher.wustl.edu"
  (let ((key ?\177)
		(pass "")
		(prompt "testing:")
		)
    ;; run until we get a cr or lf
    (while (not (or (= key ?\n) (= key ?\r)))
      (progn
		(cond
		 ;; C-g : abort
		 ((= key ?\007)
		  (error "pgp: aborted"))
		 ;; erase and delete : backup
		 ((or (= key ?\177) (= key ?\010))
		  (if (> (length pass) 0)
			  (setq pass (substring pass 0 -1))))
		 ;; C-u : kill line
		 ((= key ?\025)
		  (setq pass ""))
		 ;; all others : append char to password string
		 (t
		  (setq pass (concat pass (char-to-string key))))
		 ) ; dnoc
		
		;; display the prompt if appropriate
		(if prompt
			(message prompt))
		
		;; magic to read-key while blocking quitting while in the non-echo area
		(setq inhibit-quit t)
		(setq cursor-in-echo-area t)
		(setq key (read-char))
		(setq cursor-in-echo-area nil)
		(setq inhibit-quit nil)
		)
      )
    pass
    )
  )
(defun pgp-get-recipients ()
"Get a list of recipients for a pgp message.
reece@dasher.wustl.edu"
  (let ((recipients)
		(new-receiver t)
		(pub-key-alist (mapcar 'list (pgp-pub-key-name-list))))
    (if (and pgp-user-name
			 (or pgp-encrypt-for-sender
				 (y-or-n-p (format "Encrypt message for yourself [%s]? "
								   pgp-user-name))))
		(setq recipients (concat recipients "'" pgp-user-name "' ")))
	(while (not (string-equal new-receiver ""))
	  (setq new-receiver
			(completing-read "Encrypt message for [nil to end]: "
							 pub-key-alist nil t))
	  (if (not (string-equal "" new-receiver))
		  (setq recipients (concat recipients "'" new-receiver "' "))
		(setq new-receiver "")
		) ; fi
	  ) ; elihw
	recipients))
(defun pgp-get-user-name ()
  "get the user's name for pgp actions"
  (if (or (not pgp-user-name) pgp-always-ask-name)
	  (read-string (format "PGP user name [%s]: " pgp-user-name))
	  pgp-user-name))
(defun pgp-insert-public-key ()
  "insert pgp public key for pgp-user-name"
  (interactive)
  (let ((shell-file-name "/bin/sh"))
	(shell-command (concat pgp-executable-path 
						   " -fakx " 
						   (pgp-get-user-name)
						   " 2>/dev/null")
				   t)
	)
  )
(defun pgp-pub-key-full-list()
  "Unused, but it took me so long to get the sed string correct that I hate to
remove it.  See pgp-pub-key-name-list.
reece@dasher.wustl.edu"
  (let 
	  ((shell-file-name "/bin/sh"))
	(shell-command "pgp -kv 2>/dev/null | sed -n -e 's/ </</g' -e 's/^pub[ ]*\\([0-9]*\\)\\/........ \\([^ ]*\\) \\([^<]*\\)<\\([^>]*\\)>/\\3,\\4,\\2,\\1/gp' | nawk -v FS=, '{printf \"%-25.25s | %-25.25s | %s | %4d\\n\",$1,$2,$3,$4}' | sort")
	(make-list-from-lines "*Shell Command Output*")
	)
  )
(defun pgp-pub-key-name-list ()
"Generate a list of the user names in the pgp public keyring.
reece@dasher.wustl.edu"
  (let
	((shell-file-name "/bin/sh"))
	(shell-command "pgp -kv 2>/dev/null | sed -n -e 's/^pub[ ]*[0-9]*\\/........ [^ ]* \\(.*\\) <.*/\\1/gp' | sort")
	(make-list-from-lines "*Shell Command Output*")
	)
  )
(defun pgp-verify-signature
  "verify signature from region")

; broadcast that we're available (so to speak)

(provide 'pgp-utils)
