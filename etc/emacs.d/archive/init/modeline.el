(display-time)

; hack the mode line string to look like:
; --*scratch*       :   16/59 All : Lisp Interaction  : 00:10 0.84 gc  ---------
(defvar mode-line-location-format '("%4l/%2c " (-3 . "%p"))
  "Format for buffer position indication.  The value of this variable
replaces the default buffer position strings. See mode-line-format for
a description of string format.")
(set-default 'mode-line-modified '("%1*%1+"))
(set-default 'mode-line-format
	  '(""
 		mode-line-modified
		"%15b : "
 		mode-line-location-format
		" : %[" mode-name mode-line-process minor-mode-alist "%n %] : "
		global-mode-string
		" %-"))
