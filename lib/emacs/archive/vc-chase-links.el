;; -*- mode:emacs-lisp; mode: outline-minor -*-
;;
;; $RCSfile: vc-chase-links.el,v $
;; $Revision: 1.3 $
;; $Date: 1996/05/30 19:54:38 $
;;
;; NAME
;; vc-chase-links -- possibly follow sym links to version controlled file
;;
;; NOTE:
;; This file has been incorporated into vc-hooks.el included with 
;; GNU emacs >=19.31.
;;
;; @@banner@@
;; 

(defvar vc-chase-links "ask"
  "When loading a version controlled file that is a symbolic link,
chase the link (see file-truename) if t, don't follow if nil, or ask
if \"ask\".")

(defun vc-find-file-hook ()
"Recompute whether file is version controlled, if user has killed the
buffer and revisited.  Also chase symbolic links if necessary; see
vc-chase-links.  This function is based upon the function by the same
name in vc-hooks.el.
<96.01.19 23:36 reece@cupid.wustl.edu>"
  (cond 
   (buffer-file-name
    (vc-file-clearprops buffer-file-name)
    (cond
     ((vc-backend buffer-file-name)
      (vc-mode-line buffer-file-name)
      (cond 
	   ((not vc-make-backup-files)
		;; Use this variable, not make-backup-files,
		;; because this is for things that depend on the file name.
		(make-local-variable 'backup-inhibited)
		(setq backup-inhibited t))))

     ((let* ((link (file-symlink-p buffer-file-name))
			 (link-type (and link (vc-backend link))))
		(if link-type						; it's a link and is vc'd
			(cond
			 ((null vc-chase-links)
			  (message
			   "Warning: symbolic link to %s-controlled source file" link-type))

			 ((or (eq vc-chase-links t)
				  (and (string= vc-chase-links "ask")
					   (y-or-n-p
						(format
						 "vc: symbolic link to %s-controlled file; chase link? "
						 link-type))))
			  (buffer-revisit-as (file-truename (buffer-file-name)) t)
			  (vc-mode-line buffer-file-name))
			 ) ;dnoc
		  ) ;fi
		))))))

(provide 'vc-chase-links)
