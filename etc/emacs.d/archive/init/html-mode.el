;$Id: html-mode.el,v 1.3 1998/09/29 04:37:39 reece Exp $
;;; html-helper-mode & friends
;===============================================================================
(autoload 'html-helper-mode "html-helper-mode")
(setq auto-mode-alist (delete '("\\.s?html?\\'" . html-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.html$" . html-helper-mode) auto-mode-alist))
(add-hook 'html-helper-load-hook
		  (lambda ()
			(load-library "hhm-netscape")
			(load-library "html-tables")
			))
(add-hook 'html-helper-mode-hook
		  (lambda ()
			(outline-minor-mode)
			(make-local-variable 'outline-regexp)
			(setq outline-regexp (concat
								  "\\s-*<\\(" ; whitespace, then...
								  "[hH][1-6]" ; headings
								  "\\|[lL][Ii]"	;line items
								  "\\|[dD][tT]" ; descriptive titles
								  "\\)>"
								  ))
			))
(setq
 html-helper-use-expert-menu t
 html-helper-do-write-file-hooks nil
 html-helper-htmldtd-version "3.0"
 )

(autoload 'iso-html-minor-mode "iso-html"
  "Translate HTML to ISO 8859/1 while visiting a file." t)
;(add-hook 'html-mode-hook (function (lambda () (interactive) (iso-html-minor-mode 1))))

(if (file-readable-p "~/opt/templates/html")
	(setq html-helper-new-buffer-template
		  (list (file-contents-as-string "~/opt/templates/html"))))
;===============================================================================

