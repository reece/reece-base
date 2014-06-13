; functions
; (see bindings.el for key and mouse bindings, if any)


; adapted from http://www.tardis.ed.ac.uk/~skx/win/.emacs
(defun my-exit-from-emacs()
  (interactive)
  (if (yes-or-no-p "Do you want to exit ")
      (save-buffers-kill-emacs)))


(defun sort-words-in-region ()
  (interactive)
  (insert (mapconcat
		   'identity
		   (sort (split-string (delete-and-extract-region (point) (mark)))
				 'string<
				 )
		   " ")
          "\n")
  ) 
