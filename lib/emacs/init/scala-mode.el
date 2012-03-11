; svn export http://lampsvn.epfl.ch/svn-repos/scala/scala-tool-support/trunk/src/emacs scala-emacs

(add-to-list 'load-path (concat emacs-home-dir "/scala-emacs"))
(require 'scala-mode-auto)

(add-to-list 'load-path (concat emacs-home-dir "/ensime/elisp"))
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
