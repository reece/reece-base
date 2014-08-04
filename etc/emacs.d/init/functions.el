; functions
; (see bindings.el for key and mouse bindings, if any)


(defun delete-frame-or-kill-emacs (&optional prefix-arg)
  "Deletes the current frame.  Also kills Emacs if there's only one
   frame left.  Optional prefix PREFIX-ARG is passed to save-buffers-kill-emacs
   if that function is called, otherwise PREFIX-ARG is ignored."
  (interactive "P")
  (if (= 1 (length (frame-list)))
      (save-buffers-kill-emacs prefix-arg)
    (delete-frame)))

; adapted from http://www.tardis.ed.ac.uk/~skx/win/.emacs
(defun my-exit-from-emacs()
  (interactive)
  (if (yes-or-no-p "Do you want to exit ")
      (save-buffers-kill-emacs)))

(defun revert-buffer-noconfirm ()
  "Revert a buffer without confirmation"
  (interactive)
  (revert-buffer t t))


(defun sort-words-in-region ()
  "sort whitespace-separate words in region and replace"
  (interactive)
  (insert (mapconcat 'identity (sort (split-string (delete-and-extract-region (point) (mark))) 'string<) " ") "\n")) 
