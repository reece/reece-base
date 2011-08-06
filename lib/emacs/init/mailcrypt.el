;$Id: mailcrypt.el,v 1.2 1997/10/19 16:21:40 reece Exp $
;;; mailcrypt
;===============================================================================
(add-to-load-path (concat emacs-home-dir "/mailcrypt"))
(autoload 'mc-install-write-mode "mailcrypt" nil t)
(autoload 'mc-install-read-mode "mailcrypt" nil t)
(add-hook 'mail-mode-hook 'mc-install-write-mode)
(setq 
 mc-passwd-timeout 600						; 10 minutes
 mc-pgp-keyserver-address "pgp.ai.mit.edu"	; mc-pgp-keyserver-* for `new'
 mc-pgp-keyserver-port 11371				; keyserver (96-02-07)
 mc-pgp-keyserver-url-template "/pks/lookup?op=get&search=%s"
 )
;===============================================================================

