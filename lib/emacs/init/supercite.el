;$Id: supercite.el,v 1.1 1997/10/19 16:21:40 reece Exp $
;;; supercite
;===============================================================================
(autoload 'sc-cite-original     "supercite" "Supercite 3.1" t)
(autoload 'sc-submit-bug-report "supercite" "Supercite 3.1" t)
(add-hook 'mail-citation-hook 'sc-cite-original)
(setq sc-citation-leader "")
(setq sc-auto-fill-region-p nil)
(setq sc-preferred-attribution-list
	  (list "x-attribution" "initials" "sc-lastchoice" "firstname" "lastname"))
(setq sc-rewrite-header-list
  '((sc-no-header)
    (sc-header-on-said)
    (sc-header-inarticle-writes)
    (sc-header-regarding-adds)
    (sc-header-attributed-writes)
    (sc-header-author-writes)
    (sc-no-blank-line-or-header)
	(sc-header-Reece)
	(sc-header-elements)
    ))
(setq sc-preferred-header-style 7)
(setq sc-electric-references-p t)

(defun sc-hdr-Reece (prefix text suffix &optional IFEMPTY)
  (if (or (null text) (string= text ""))
	  (or IFEMPTY "")
	  (concat sc-reference-tag-string prefix text suffix)))

(defun sc-header-Reece ()
  (let ((sc-mumble ""))
	(insert
	 (sc-hdr-Reece (concat "" (sc-mail-field "sc-attribution") " == ")
				   (sc-whofrom)
				   " wrote \n")
	 (sc-hdr-Reece "In "
				   (sc-mail-field "newsgroups") 
				   "\n")
	 (sc-hdr-Reece "On "
				   (sc-mail-field "date")
				   "\n")
	 (sc-hdr-Reece "Re "
				   (sc-mail-field "subject")
				   "\n")
	 )))

(defun sc-header-elements ()
"fake eref header to see what the fields look like.  For example:
date:06 Dec 1995 09:19:13 -0600
message-id:<82u43ew77y.fsf@cupid.wustl.edu>
newsgroups:wu.test
organization:Biophysics & Biochemistry, Washington University, St. Louis, USA
references:
subject:test 1
sc-attribution:reece
sc-author:Reece Kimball Hart
sc-citation:reece> 
sc-emailname:reece
sc-firstname:Reece
sc-from-address:reece@cupid.wustl.edu
sc-initials:RKH
sc-lastname:Hart
sc-middlename-1:Kimball
sc-reply-address:
sc-sender-address:reece@cupid.wustl.edu
whofrom:reece@cupid.wustl.edu (Reece Kimball Hart)"
  (let ((sc-mumble ""))
	(insert
	 "date:" (sc-mail-field "date") "\n"
	 "message-id:" (sc-mail-field "message-id") "\n"
	 "newsgroups:" (sc-mail-field "newsgroups") "\n"
	 "organization:" (sc-mail-field "organization") "\n"
	 "references:" (sc-mail-field "references") "\n"
	 "subject:" (sc-mail-field "subject") "\n"
	 "sc-attribution:" (sc-mail-field "sc-attribution") "\n"
	 "sc-author:" (sc-mail-field "sc-author") "\n"
	 "sc-citation:" (sc-mail-field "sc-citation") "\n"
	 "sc-emailname:" (sc-mail-field "sc-emailname") "\n"
	 "sc-firstname:" (sc-mail-field "sc-firstname") "\n"
	 "sc-from-address:" (sc-mail-field "sc-from-address") "\n"
	 "sc-initials:" (sc-mail-field "sc-initials") "\n"
	 "sc-lastname:" (sc-mail-field "sc-lastname") "\n"
	 "sc-middlename-1:" (sc-mail-field "sc-middlename-1") "\n"
	 "sc-reply-address:" (sc-mail-field "sc-reply-address") "\n"
	 "sc-sender-address:" (sc-mail-field "sc-sender-address") "\n"
	 "whofrom:" (sc-whofrom) "\n"
	 )))
;===============================================================================

