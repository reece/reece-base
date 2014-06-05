;; -*- mode:emacs-lisp; mode: outline-minor -*-
;;
;; Rtrinkets -- miscellaneous emacs code
;; 

(defconst bar (make-string 80 45))			; 45 == '-'

(defvar prefix-function-name t
  "Prefix errors and messages with function name in which they originate.")

;; Are we running XEmacs or Emacs?  Oh, why can't we call get along ?
(defvar running-xemacs (string-match "XEmacs\\|Lucid" emacs-version))

(defun add-to-load-path (elems)
  "Prepend elems (list or symbol) to load-path if not already there."
  ; should use (add-to-list ...)?
  (setq load-path (prepend-if-not-member elems load-path)))

(defun banner (PFX TEXT)
  (interactive "s1:\ns2:")
  (shell-command (concat "banner " TEXT "|sed -e 's/^/" PFX "/'") t))

(defun buffer-revisit-as (FILE &optional WARP)
  "buffer-revisit-as-file (FILE)
Change the filename that the current buffer is visiting to FILE.  If
FILE is already being visited by another buffer, then kill this buffer
and go there if WARP is t, otherwise error.  The buffer name is
returned.  A buffer's visiting file comparison is based upon the
actual file being modified after chasing links."
  (interactive)
  (if (string= FILE (buffer-file-name))
	  ;; then
	  (buffer-name)
	;; else
	(let ((visiting-same-file (find-buffer-visiting FILE))
		  (buffer-modified (buffer-modified-p)))
	  
	  (if (and visiting-same-file
			   (not (string= (buffer-name) (buffer-name visiting-same-file))))
		  (if WARP
			  (progn
				(kill-buffer (buffer-name))
				(switch-to-buffer visiting-same-file)
				(buffer-name visiting-same-file)
				)
			
			(error "%sbuffer %s already visiting %s"
				   (if prefix-function-name "buffer-revisit-as: " "")
				   (buffer-name visiting-same-file)
				   FILE
				   )
			)								;fi WARP
		;; else
		(progn
		  (set-visited-file-name FILE)
		  (set-buffer-modified-p buffer-modified)
		  (buffer-name))
		)									;fi
	  )
	)
  )

(defun cut-here ()
  "Insert a delineate a form which follows."
  (interactive)
  (insert-file-contents "~reece/notes/scissors"))

(defun delete-frame-or-kill-emacs (&optional prefix-arg)
  "Deletes the current frame.  Also kills Emacs if there's only one
   frame left.  Optional prefix PREFIX-ARG is passed to save-buffers-kill-emacs
   if that function is called, otherwise PREFIX-ARG is ignored."
  (interactive "P")
  (if (= 1 (length (frame-list)))
      (save-buffers-kill-emacs prefix-arg)
    (delete-frame)))

(defun dir-read-write-p (path)
  "return t if path exists, is an accessible directory, and is
writable; otherwise nil"
  (and path (file-accessible-directory-p path) (file-writable-p path)))

(defun dired-with-buffer-name (BUFNAME DIRNAME &optional SWITCHES)
  (interactive)
  (dired DIRNAME)
  (rename-buffer BUFNAME t)
  )
(defun dired-home ()
 (interactive)
 (if (get-buffer "~")
	 (switch-to-buffer "~")
   (dired-with-buffer-name "~" "~/[^.]*")))

(defun dissociate-buffer-from-file ()
  "Dissociates buffer from its visited file so that changes will not be
saved.  This allows files to be used as templates easily and without
making the file read only."
  (interactive)
  (set-visited-file-name nil))

(defun dot-dot-dot ()
  "Vertical elipsis in text to indicated deleted region."
  (interactive)
  (insert "  .\n  . [text deleted]\n  .\n"))

(defun exclude-region (start end)
  "Comment out a region with a #if 0/#endif pair."
  (interactive "r")
  (goto-char end)
  (beginning-of-line)
  (insert "#endif\n")
  (goto-char start)
  (beginning-of-line)
  (insert "#if 0\n")
  (goto-char end)
  (next-line 2))

(defun file-contents-as-string (FILE &optional BEG END)
  "Return the contents of FILE, between BEG and END (see
insert-file-contents for description)."
  (if (not (file-readable-p FILE))
	  (error "%s not readable" FILE))
  (let ((buf (generate-new-buffer " fcas"))
		bs
		beg
		end)
	(set-buffer buf)
	(insert-file-contents FILE nil BEG END)
	(setq bs (buffer-substring (point-min) (point-max)))
	(kill-buffer buf)
	bs))

(defun prepend-if-not-member (elems list)
  "Prepends each of the elements of elems (either a list or a symbol)
to list, returning the result"
  (cond
   ((null elems)							; null element
	list)

   ((listp elems)							; elems is a list
	  (append (prepend-if-not-member		; process each car in turn
			   (car elems)
			   (prepend-if-not-member (cdr elems) list))))

   ((member elems list)						; elems already there
   list)									; don't add it
	
   (t										; otherwise prepend it
	(append (list elems) list))
   ))

(defun rewrite-date (date-string)
"Convert dates to a common format. e.g.,
Mon, 4 Dec 1995 13:21:59 -0600 (CST)  -> 1995 Dec 04 (Mon) 13:21:59 -0600 (CST)
Mon,  4 Dec 1995 13:21:59 -0600 (CST) -> 1995 Dec 04 (Mon) 13:21:59 -0600 (CST)
Mon 4 Dec 1995 13:21:59 -0600 (CST)   -> 1995 Dec 04 (Mon) 13:21:59 -0600 (CST)
Mon, 04 Dec 1995 13:21:59 -0600 (CST) -> 1995 Dec 04 (Mon) 13:21:59 -0600 (CST)
Mon, 4 Dec 1995 13:21:59 -0600        -> 1995 Dec 04 (Mon) 13:21:59 -0600 
Mon, 4 Dec 1995 13:21:59 (CST)        -> 1995 Dec 04 (Mon) 13:21:59 (CST)
Mon, 4 Dec 1995 13:21:59              -> 1995 Dec 04 (Mon) 13:21:59 
Tue,  5 Dec 95 17:21:44 -600          -> 1995 Dec 05 (Tue) 17:21:44 -0600
"
   (if (string-match "^\\(Mon\\|Tue\\|Wed\\|Thu\\|Fri\\|Sat\\|Sun\\|\\),?[ ]?\\([0-9 ][0-9]?\\) \\(Jan\\|Feb\\|Mar\\|Apr\\|May\\|Jun\\|Jul\\|Aug\\|Sep\\|Oct\\|Nov\\|Dec\\) \\(1?9?9[5-9]\\) \\([0-9][0-9]:[0-9][0-9]:[0-9][0-9]\\)[ ]?\\([+-]\\)?\\([0-9][0-9][0-9][0-9]?\\|\\)?[ ]?(?\\([A-Z][A-Z][A-Z]\\|\\))?$" date-string)
	   (concat
		(let ((year (string-to-int (match-string 4 date-string))))
		  (if (> year 1900) year (+ 1900 year))) " "
		(match-string 3 date-string) " "
		(format "%02d " (string-to-int (match-string 2 date-string)))
		(if (match-string 1 date-string)
			(concat "(" (match-string 1 date-string) ") "))
		(match-string 5 date-string) " "
		(if (not (string-equal (match-string 7 date-string) "")) ; TZ
			(concat (match-string 6 date-string) ; sign
					(format "%04d " (string-to-int ; value
									(match-string 7 date-string)))))
		(if (not (string-equal (match-string 8 date-string) "")) ; TZ
			(concat "("
					(match-string 8 date-string)
					")"))
		)
	 )
   )
; test for rewrite-date
;(setq dates (list "Mon, 4 Dec 1995 13:21:59 -0600 (CST)"
;				  "Mon,  4 Dec 1995 13:21:59 -0600 (CST)"
;				  "Mon 4 Dec 1995 13:21:59 -0600 (CST)"
;				  "Mon, 04 Dec 1995 13:21:59 -0600 (CST)"
;				  "Mon, 4 Dec 1995 13:21:59 -0600"
;				  "Mon, 4 Dec 1995 13:21:59 (CST)"
;				  "Mon, 4 Dec 1995 13:21:59"
;				  "Tue,  5 Dec 95 17:21:44 -600"))
;(insert (mapcar (function (lambda (d) (insert (format "\n;%-37s -> %s" d (sc-rewrite-date d))))) dates))

(defun revert-buffer-noconfirm ()
  "Revert a buffer without confirmation"
  (interactive)
  (revert-buffer t t))

(defun tab-width-8-local ()
  "make current buffer only tab width 8"
  (interactive)
  (make-local-variable 'tab-width)
  (set-variable 'tab-width 8)
  )

(defun tmpdir ()
"Returns a valid directory into which temporary files may be written, or
nil if unable to find such a directory.  The search order is TMPDIR
 (environment variable), `/tmp', `.'; then failure.  See tmpfn.
reece@in-machina.com"
  (interactive)
  (let ((dirs (list (getenv "TMPDIR") "/tmp" ".")))
	(while (and (car dirs) (not (dir-read-write-p (car dirs))))
	  (setq dirs (cdr dirs)))
	(car dirs)
	)
  )

(defun tmpfn (&optional prefix create tmpdir)
"tmpfn (&optional prefix create tmpdir)
Returns the full path of a valid temporary file. The filename is
created by concatenating tmpdir, prefix, and a random number until a
unique name is found.  If prefix is nil, it defaults to `tmpfn'.  If
tmpdir is nil, it defauls to the result returned by (tmpdir).  If
create is t, then the file created (with zero size and permissions
according to the current umask) to prevent subsequent calls to tmpfn
from returning the same name.  In the rare event that there are
1000000 files with names of this form, this function will fail to
return.  See tmpdir.
reece@in-machina.com"
  (if tmpdir
	  (if (not (dir-read-write-p tmpdir))
		  (error
		   (format "tmpfn: %s nonexistant or incorrect permissions" tmpdir)))
	(if (not (setq tmpdir (tmpdir)))
		(error "tmpfn: can't find a valid directory for temporary files.  See tmpdir.")))
  (let ((path-prefix (concat tmpdir "/" 
							 (if prefix prefix "tmpfn") "-"))
		(fn))
	(while (or (not fn) (file-exists-p fn))
	  (setq fn (format "%s%06d" path-prefix (random 999999)))
	  )
	(if create (write-region 1 1 fn))
	fn)
  )

(defun untabify-message ()
  "untabify body of message before sending"
  (interactive)
  (save-excursion
	(goto-char (point-min))
	(search-forward-regexp "^---" (point-max) t)
	(next-line 1)
	(beginning-of-line)
	(let ((beg (point)))
	  (goto-char (point-max))
	  (untabify beg (point))
	  )
	)
  )

(defun sort-words-in-region ()
  "sort whitespace-separate words in region and replace"
  (interactive)
  (insert (mapconcat
		   'identity
		   (sort (split-string (delete-and-extract-region (point) (mark)))
				 'string<
				 )
		   " ")
          "\n")
  ) 

(provide 'Rtrinkets)
