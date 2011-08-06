;$Id: font-lock.el,v 1.3 2007/10/15 18:47:49 rkh Exp $
;;; font-lock
;===============================================================================
(or running-xemacs (global-font-lock-mode t))
(setq font-lock-support-mode 'lazy-lock-mode)

;; modify some faces
;; these are inspired mostly because my monitor is a little faint
;; face-lock  -- allow colors, grayscale in addition to fonts, styles
(eval-after-load
 "font-lock"
 '(progn
	  (set-face-foreground 'font-lock-string-face "Magenta")
	  (make-face-bold 'font-lock-keyword-face)
	  (make-face-bold 'font-lock-type-face)
	  ;;(make-face-bold 'font-lock-other-type-face)
;	  (require 'choose-color)
	  (global-set-key "\C-cl"		'font-lock-fontify-buffer) ;redraw display
	  ))
;===============================================================================

