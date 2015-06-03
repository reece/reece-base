(require 'package)

(add-to-list 'package-archives '("elpy"      . "http://jorgenschaefer.github.io/packages/"))
(add-to-list 'package-archives '("gnu"       . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa"     . "http://melpa.milkbox.net/packages/"))

(package-initialize)

(defvar packages '(
		   ;auto-complete
		   ;coffee-mode
		   csv-mode
		   ;jedi
		   yasnippet
		   fuzzy
		   ; python-mode
		   )
  "A list of packages to ensure are installed at launch.")

(when (not package-archive-contents)
  (package-refresh-contents))

(dolist (p packages)
  (when (not (package-installed-p p))
    (package-install p)))
