;$Id: time-stamp.el,v 1.3 1998/09/29 04:38:08 reece Exp $
;;; time-stamp
;===============================================================================
(require 'time-stamp)
(defun insert-time-stamp ()
  "Insert a time stamp at point.  See time-stamp for more information."
  (interactive)
  (insert-string (time-stamp-string)))
(setq time-stamp-format (format "%%:y/%%02m/%%02d %%02H:%%02m (%s)"
								(user-real-login-name)
								)) ;(system-name)
;===============================================================================
