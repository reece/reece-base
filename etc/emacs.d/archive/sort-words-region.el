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
