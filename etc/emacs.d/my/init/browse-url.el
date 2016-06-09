(setq
 ;; browse-url-browser-function (quote browse-url-generic)
 browse-url-browser-function (list
			      '("\\.md$" . browse-url-generic)
			      '("" . browse-url-xdg-open) ; fallback
			      )
 browse-url-generic-program "google-chrome"
)


