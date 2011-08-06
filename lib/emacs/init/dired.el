;$Id: dired.el,v 1.3 1998/03/09 00:51:54 reece Exp $
;;; dired, dired-x
;===============================================================================
(and (not running-xemacs)
	 (add-hook 'dired-load-hook
			   (function (lambda ()			; bind dired-x-find-file over find-file
						   (setq dired-x-hands-off-my-keys nil)
						   (load "dired-x")
						   )))
	 )
(setq dired-guess-shell-alist-user
	  (list (list "\\.html$" "open-url")))	; open-url is a Reece-ism
(defun Reece-dired-open-info-node ()
  (interactive)
  (Info-goto-node (concat "(" (dired-get-filename) ")")))
(defun Reece-dired-setup ()
  (autoload 'Info-goto-node "info")			; Info may not be loaded yet
  (local-set-key "I" 'Reece-dired-open-info-node))
(add-hook 'dired-mode-hook 'Reece-dired-setup)
;===============================================================================
 
