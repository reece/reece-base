;$Id: shell.el,v 1.3 1999/03/18 13:05:41 reece Exp $
;;; cc-mode
;===============================================================================
(defun reece-shell-mode-setup ()
  (interactive)
  (outline-minor-mode)
  (make-local-variable 'outline-regexp)
  (setq outline-regexp "\\$")
  (setq tab-width 8)
  )
(add-hook 'shell-mode-hook 'reece-shell-mode-setup)
;===============================================================================
