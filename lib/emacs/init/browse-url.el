;$Id: browse-url.el,v 1.3 2007/10/15 20:13:23 rkh Exp $
;;; browse-url
;===============================================================================
(autoload 'browse-url-at-point "browse-url"
   "Ask a WWW browser to load the URL at or before point." t)
(autoload 'browse-url-at-mouse "browse-url"
  "Ask a WWW browser to load a URL clicked with the mouse." t)
(autoload 'browse-url-of-buffer "browse-url"
  "Ask a WWW browser to display BUFFER." t)
(autoload 'browse-url-of-file "browse-url"
  "Ask a WWW browser to display FILE." t)
(autoload 'browse-url-of-dired-file "browse-url"
  "In Dired, ask a WWW browser to display the file named on this line." t)

(setq
 browse-url-browser-function (quote browse-url-generic)
 browse-url-generic-program "xdg-open"
)

(add-hook 'dired-mode-hook
		  (function (lambda ()
					  (local-set-key "\C-cuf" 'browse-url-of-dired-file))))
(if (boundp 'browse-url-browser-function)
    (global-set-key "\C-cuu" browse-url-browser-function)
  (eval-after-load
   "browse-url"
   '(global-set-key "\C-cuu" browse-url-browser-function)))
(setq browse-url-save-file t)				; save before browsing
;===============================================================================

