;$Id: tex-mode.el,v 1.7 2000/08/14 02:17:39 reece Exp $
(defun reece-tex-mode-setup ()
  (setq
   tex-dvi-print-command "dvips * -o"
   TeX-lisp-directory (concat emacs-home-dir "/auctex")
;   TeX-macro-global "/local/teTeX/texmf/tex"
   TeX-print-command "dvips %s -P%p -o-"
   TeX-parse-self t							; Enable parse on load.
   TeX-auto-save t							; Enable parse on save.
   TeX-auto-regexp-list TeX-auto-full-regexp-list
   )
  (outline-minor-mode)
  (auto-fill-mode 1)
  )
;(require 'tex-site)							; auxtex
(add-hook 'TeX-mode-hook 'reece-tex-mode-setup)

