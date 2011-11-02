; svn export http://lampsvn.epfl.ch/svn-repos/scala/scala-tool-support/trunk/src/emacs scala-emacs

(add-to-list 'load-path (concat emacs-home-dir "/scala-emacs"))
(require 'scala-mode-auto)
