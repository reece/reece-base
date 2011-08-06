;;
;; $RCSfile: popchar.el,v $
;; $Revision: 0.13 $
;; $Date: 2007/10/15 18:47:49 $
;;
;; NAME
;; popchar.el -- accented character selection via popup buffer
;;
;; The idea and name for this were shamelessly stolen from popchar for the
;; Macintosh written by Günter Blaschek <gue@soft.uni-linz.ac.at>
;; <URL:http://infosoft.soft.uni-linz.ac.at/Staff:Blaschek.html>
;;
;; @@banner@@
;; 

;; $Log: popchar.el,v $
;; Revision 0.13  2007/10/15 18:47:49  rkh
;; commit from snafu
;;
;; Revision 0.12  1997/02/11 02:40:47  reece
;; *** empty log message ***
;;
;; Revision 0.11  1996/05/20 22:45:08  reece
;; * added [space] to exit popup buffer
;; * simple commenting changes
;;
;; Revision 0.9  1996/02/10 03:07:16  reece
;; * popchar is now inset, just large enough to display the characters
;;
;; Revision 0.8  1996/02/10 02:31:13  reece
;; * removed \200-\245 (or so) because they caused hilighting problems and
;;   were of unknown use
;; * added {beginning,end}-of-{line,buffer} functions and bindings
;; * added popchar-exit-no-select
;; * added bindings for C-g, C-xk, and q to quit without selection

(defvar popchar-chars-per-line 40
  "Number of characters per line in popchar buffer")

(defvar popchar-keymap nil
  "keymap for popchar buffer")

(defvar popchar-translation-alist
  '(
	;; symbols
	("µ"."M-5") ("ß"."M-_") ("Ø"."M-X") ("ø"."M-x") ("¡"."M-!")
	("¿"."M-?") ("¢"."M-\"") ("£"."M-#") ("¶"."M-6") ("§"."M-'")
	("¥"."M-%") ("¦"."M-&") ("©"."M-)") ("®"."M-.") ("«"."M-+")
	("»"."M-;") ("¬"."M-,") ("¤"."M-$") ("±"."M-1") ("­"."M--")
	("¯"."M-/") ("÷"."M-w") ("×"."M-W") ("·"."M-7") ("°"."M-0")
	("¨"."M-(") ("´"."M-4") ("¸"."M-8") ("ª"."M-*") ("º"."M-:")
	("¹"."M-9") ("²"."M-2") ("³"."M-3") ("¼"."M-<") ("½"."M-=")
	("¾"."M->")

	;; accented and non-Roman letters
	("À"."M-@") ("Á"."M-A") ("Â"."M-B") ("Ã"."M-C") ("Ä"."M-D")
	("Å"."M-E") ("Æ"."M-F") ("à"."M-`") ("á"."M-a") ("â"."M-b")
	("ã"."M-c") ("ä"."M-d") ("å"."M-e") ("æ"."M-f") ("Ç"."M-G")
	("ç"."M-g") ("È"."M-H") ("É"."M-I") ("Ê"."M-J") ("Ë"."M-K")
	("è"."M-h") ("é"."M-i") ("ê"."M-j") ("ë"."M-k") ("Ì"."M-L")
	("Í"."M-M") ("Î"."M-N") ("Ï"."M-O") ("ì"."M-l") ("í"."M-m")
	("î"."M-n") ("ï"."M-o") ("Ñ"."M-Q") ("ñ"."M-q") ("Ò"."M-R")
	("Ó"."M-S") ("Ô"."M-T") ("Õ"."M-U") ("Ö"."M-V") ("ò"."M-r")
	("ó"."M-s") ("ô"."M-t") ("õ"."M-u") ("ö"."M-v") ("Ù"."M-Y")
	("Ú"."M-Z") ("Û"."M-[") ("Ü"."M-\\") ("ù"."M-y") ("ú"."M-z")
	("û"."M-{") ("ü"."M-|") ("Ý"."M-]") ("ý"."M-}") ("Ð"."M-P")
	("ð"."M-p") ("Þ"."M-^") ("þ"."M-~") ("ÿ"."M-^?")

	;; Uncomment the following lines if you want common characters to appear
	;; in the popup
	;; ("!"."!") (""."^?") ("\""."\"") ("#"."#") ("$"."$") ("%"."%")
	;; ("&"."&") ("'"."'") ("("."(") (")".")") ("*"."*") ("+"."+")
	;; (",".",") ("-"."-") (".".".") ("/"."/") (":".":") (";".";")
	;; ("<"."<") (">".">") ("?"."?") ("@"."@") ("["."[") ("\""."\"")
	;; ("]"."]") ("^"."^") ("_"."_") ("`"."`") ("{"."{") ("|"."|")
	;; ("}"."}") ("~"."~")

	;; ("0"."0") ("1"."1") ("2"."2") ("3"."3") ("4"."4") ("5"."5")
	;; ("6"."6") ("7"."7") ("8"."8") ("9"."9")

	;; ("A"."A") ("B"."B") ("C"."C") ("D"."D") ("E"."E") ("F"."F")
	;; ("G"."G") ("H"."H") ("I"."I") ("J"."J") ("K"."K") ("L"."L")
	;; ("M"."M") ("N"."N") ("O"."O") ("P"."P") ("Q"."Q") ("R"."R")
	;; ("S"."S") ("T"."T") ("U"."U") ("V"."V") ("W"."W") ("X"."X")
	;; ("Y"."Y") ("Z"."Z")

	;; ("a"."a") ("b"."b") ("c"."c") ("d"."d") ("e"."e") ("f"."f")
	;; ("g"."g") ("h"."h") ("i"."i") ("j"."j") ("k"."k") ("l"."l")
	;; ("m"."m") ("n"."n") ("o"."o") ("p"."p") ("q"."q") ("r"."r")
	;; ("s"."s") ("t"."t") ("u"."u") ("v"."v") ("w"."w") ("x"."x")
	;; ("y"."y") ("z"."z")
	)
  "alist of ( \"character\" . \"keypress string\" )"
  ) ; popchar-translation-alist


(defvar popchar-current-index 0 "index into popchar-translation-alist")
(defconst popchar-buffer-name " *PopChar*")
(defvar popchar-keymap nil
  "keymap for popchar character selection buffer.")
(progn
  (setq popchar-keymap (make-sparse-keymap))
  (define-key popchar-keymap [return]	'popchar-exit)
  (define-key popchar-keymap [space]	'popchar-exit)
  (define-key popchar-keymap "\C-xk"	'popchar-exit-no-select)
  (define-key popchar-keymap "\C-g"		'popchar-exit-no-select)
  (define-key popchar-keymap "q"		'popchar-exit-no-select)
  (define-key popchar-keymap "Q"		'popchar-exit-no-select)
  (define-key popchar-keymap [home]		'popchar-beginning-of-buffer)
  (define-key popchar-keymap [end]		'popchar-end-of-buffer)
  (define-key popchar-keymap [up]		'popchar-prev-line)
  (define-key popchar-keymap [down]		'popchar-next-line)
  (define-key popchar-keymap [left]		'popchar-prev-char)
  (define-key popchar-keymap [right]	'popchar-next-char)
  (define-key popchar-keymap "\C-a"		'popchar-beginning-of-line)
  (define-key popchar-keymap "\C-e"		'popchar-end-of-line)
  (define-key popchar-keymap "\C-p"		'popchar-prev-line)
  (define-key popchar-keymap "\C-n"		'popchar-next-line)
  (define-key popchar-keymap "\C-b"		'popchar-prev-char)
  (define-key popchar-keymap "\C-f"		'popchar-next-char)
  )

(defun popchar-insert-char ()
  (interactive)
  (barf-if-buffer-read-only)
  (insert (popchar-select-char)))

(defun popchar-select-char ()
  (let ((pc-buffer (popchar-setup-buffer))
		(char-to-insert 0)
;		(message-log-max-saved message-log-max)
	  )
	(save-window-excursion
	  ;; put popchar inside a small window inset in the current window
      (split-window nil (- (window-height) 
						   (max 4 (/ (1- (length popchar-translation-alist))
									 popchar-chars-per-line))))
	  (other-window 1)
	  (switch-to-buffer pc-buffer t)

	  (popchar-goto-index 0)
;	  (setq message-log-max nil)			; disable message logging
	  (recursive-edit)
;	  (setq message-log-max message-log-max-saved)
	  (setq char-to-insert (popchar-current-char))
	  (kill-buffer pc-buffer)
	  char-to-insert
	  )))

(defun popchar-setup-buffer (&optional XL-ALIST)
  "Setup popchar buffer with characters, local keymap, and buffer settings."
  (or (get-buffer popchar-buffer-name)
	  (let ((pc-buffer (get-buffer-create popchar-buffer-name))
			)
		(set-buffer pc-buffer)

		(kill-all-local-variables)

		(make-local-variable 'popchar-translation-alist)
		(if XL-ALIST (setq popchar-translation-alist XL-ALIST))

		(make-local-variable 'popchar-current-index)
		(setq popchar-current-index 0)

		(mapcar '(lambda (CHAR) (progn
								  (insert
								   (car CHAR)
								   (if (> (current-column)
										  (- (* 2 popchar-chars-per-line) 3))
									   "\n" " "))))
				(or XL-ALIST popchar-translation-alist))

		(buffer-disable-undo)
		(toggle-read-only t)
		(use-local-map popchar-keymap)

		pc-buffer
		)))

(defun popchar-current-char ()
  "Returns the character currently selected in the popchar buffer as a
  string, or \"\" if non selected"
  (if (= popchar-current-index -1)
	  ""
	(car (nth popchar-current-index popchar-translation-alist))))

(defun popchar-how-to-string (CHAR)
  "(popchar-how-to-string CHAR)
return informative string describing required keypress for specifed
CHAR.  The user may have to type \\[quoted-insert] (quoted-insert) to
use this key sequence."
  (format "Type %s %s to get %s."
		  (substitute-command-keys "\\[quoted-insert]")
		  (popchar-kbd-equivalent CHAR)
		  CHAR))

(defun popchar-kbd-equivalent (CHAR)
  "(popchar-kbd-equivalent CHAR)
return string representing keypress required to get specified CHAR."
  (cdr (assoc CHAR popchar-translation-alist)))

(defun popchar-index-to-marker (INDEX)
  "Return buffer char position given index of character in translation table."
  (+ (* INDEX 2) 1))

(defun popchar-marker-to-index (MARKER)
  "Return index of character in translation table given buffer char position."
  (/ MARKER 2))

(defun popchar-goto-index (INDEX)
  (interactive "nIndex: ")
  (if (or (< INDEX 0) (> INDEX (1- (length popchar-translation-alist))))
	  (error "popchar-goto-index: Index out of range (%d <= %d <= %d)"
			 0 INDEX (1- (length popchar-translation-alist)))
	)
  (setq popchar-current-index INDEX)
  (goto-char (popchar-index-to-marker popchar-current-index))
  ;; "%s" is req'd else strings with % in the kbd sequence cause barfing
  (message "%s" (popchar-how-to-string (popchar-current-char)))
  )

(defun popchar-goto-nearest ()
  (popchar-goto-index (min (popchar-marker-to-index (1- (point)))
						   (1- (length popchar-translation-alist)))))

(defun popchar-exit ()
  (interactive)
  (throw `exit nil))

(defun popchar-exit-no-select ()
  (interactive)
  (setq popchar-current-index -1)
  (throw `exit nil))

(defun popchar-beginning-of-buffer ()
  (interactive)
  (beginning-of-buffer)
  (popchar-goto-nearest))

(defun popchar-end-of-buffer ()
  (interactive)
  (end-of-buffer)
  (popchar-goto-nearest))

(defun popchar-beginning-of-line ()
  (interactive)
  (beginning-of-line)
  (popchar-goto-nearest))

(defun popchar-end-of-line ()
  (interactive)
  (end-of-line)
  (popchar-goto-nearest))

(defun popchar-prev-char ()
  (interactive)
  (cond
   ((> popchar-current-index 0)
	(setq popchar-current-index (- popchar-current-index 1))
	(popchar-goto-index popchar-current-index))
   (t
	(beep))
   ))
(fset 'popchar-backward-char 'popchar-prev-char)

(defun popchar-next-char ()
  (interactive)
  (cond
   ((< popchar-current-index (- (length popchar-translation-alist) 1))
	(setq popchar-current-index (+ popchar-current-index 1))
	(popchar-goto-index popchar-current-index))
   (t
	(beep))
   ))
(fset 'popchar-forward-char 'popchar-next-char)

(defun popchar-prev-line ()
  (interactive)
  (setq popchar-current-index (- popchar-current-index popchar-chars-per-line))
  (cond
   ((< popchar-current-index 0)
	(setq popchar-current-index 0))
   ((> popchar-current-index (- (length popchar-translation-alist) 1))
	(setq popchar-current-index (- (length popchar-translation-alist) 1)))
   )
  (popchar-goto-index popchar-current-index)
  )

(defun popchar-next-line ()
  (interactive)
  (setq popchar-current-index (+ popchar-current-index popchar-chars-per-line))
  (cond
   ((< popchar-current-index 0)
	(setq popchar-current-index 0))
   ((> popchar-current-index (- (length popchar-translation-alist) 1))
	(setq popchar-current-index (- (length popchar-translation-alist) 1)))
   )
  (popchar-goto-index popchar-current-index)
  )

(provide 'popchar)
