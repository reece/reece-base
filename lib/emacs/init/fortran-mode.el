;$Id: fortran-mode.el,v 1.2 1998/02/17 05:05:24 reece Exp $
(defun reece-fortran-mode-setup ()
 (setq
  fortran-do-indent 2
  fortran-if-indent 2
  fortran-continuation-indent 2
  fortran-line-number-indent 5				; right justify
  fortran-blink-matching-if t
  fortran-continuation-string "&"
  fortran-comment-indent-style 'relative

  fortran-font-lock-keywords fortran-font-lock-keywords-2

  comment-column 44
  comment-start "! "

  ;; enlightened f77 compilers allow cpp directives; `#' is NOT a comment start:
  comment-line-start-skip	"^[Cc*]\\(\\([^ \t\n]\\)\\2\\2*\\)?[ \t]*"
  )
 (local-set-key "\C-c\C-r"	'revert-buffer-noconfirm)
)

(add-hook 'fortran-mode-hook 'reece-fortran-mode-setup)

(setq auto-mode-alist
	  (append auto-mode-alist (list '("\\.i$" . fortran-mode)
									'("\\.inc$" . fortran-mode))))
