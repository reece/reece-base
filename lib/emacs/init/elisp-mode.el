;$Id: elisp-mode.el,v 1.1 1997/10/19 16:21:40 reece Exp $
;;; elisp-mode setup
;===============================================================================
(defun reece-emacs-lisp-mode-setup ()
  (set-variable 'comment-column 44)
  (outline-minor-mode)
  )
(add-hook 'emacs-lisp-mode-hook 'reece-emacs-lisp-mode-setup)
;===============================================================================

