;$Id: bibtex-mode.el,v 1.1 1997/10/19 16:21:40 reece Exp $
(defun reece-bibtex-mode-setup ()
 (setq
  bibtex-autokey-name-year-separator ":"
  bibtex-autokey-year-title-separator ":"
  bibtex-autokey-year-length 4
  bibtex-autokey-titlewords 0
  )
 (outline-minor-mode)
)
(add-hook 'bibtex-mode-hook 'reece-bibtex-mode-setup)
