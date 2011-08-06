;; -*- mode:emacs-lisp; mode: outline-minor -*-
;;
;; $RCSfile: shebang.el,v $
;; $Revision: 1.4 $
;; $Date: 2007/10/15 18:47:49 $
;;
;; NAME
;; shebang.el -- check for valid #! interpreter and set executable mode
;;
;; @@banner@@
;; 

(defvar shebang-file "ask"
  "See shebang-chmod.")
(make-variable-buffer-local 'shebang-file)

(defvar shebang-check-interpreter t
  "Verify that the interpreter exists.")

; if string, then assumed to be a+x 
; make this shebang-exec-modes
(defvar shebang-modes 493
  "The permissions used for executable files.  A typical value is 493
(755 octal) for rwxr-xr-x permissions.  Note that the value must be an
integer.")

(defun shebang-chmod ()
  "If file starts with `#!' then possibly chmod the file to the perms
specified by the variable shebang-modes.  If shebang-file is nil, the perms
are not changed.  If shebang-file is t, perms are always changed.  If
shebang-file is \"ask\", the user will be prompted."
  (interactive)
  (let ((truename (file-truename (buffer-file-name))))
	(and
	 (save-excursion					; file is shebang'd
	   (goto-char 1)
	   (looking-at "#!"))
;	 (not (file-executable-p truename))	; and not already executable
	 (not (= (file-modes truename) shebang-modes))
	 (if (string= shebang-file "ask")		; and shebang-file preference
		 (y-or-n-p
		  (format
		   "%sChange mode of %s to %o? "
		   (if prefix-function-name "shebang: " "")
		   (buffer-file-name)
		   shebang-modes
		   ))
	   shebang-file)

	 ;; then set mode
	 (set-file-modes (file-truename (buffer-file-name)) shebang-modes)
	 )
	)
  ;; don't ask again
  (setq shebang-file nil)
  )
(add-hook 'after-save-hook 'shebang-chmod)

(defun shebang-check-interpreter ()
  (interactive)
  (save-excursion					; file is shebang'd
	(goto-char 1)
	(if
		(re-search-forward "^#![ ]?\\([^ 	\n]+\\)" 80 t) ; int. < 50-2 chars
		(let ((interpreter (buffer-substring (match-beginning 1) (match-end 1))))
		  (if (or 
				;; doesn't begin with /
				(not (= (string-match "/" interpreter) 0))
				;; non (existent and executable)
				(not (and (file-exists-p interpreter)
						  (file-executable-p interpreter))))
			  (message
			   "%sWarning: `%s' is not a valid interpreter."
			   (if prefix-function-name "shebang: " "")
			   interpreter)
			)
		  )
	  )
	)
  )
(add-hook 'after-save-hook 'shebang-check-interpreter)

(provide 'shebang)
