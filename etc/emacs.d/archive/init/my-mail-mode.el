(defun my-mail-mode ()
  ""
  (interactive)
  (add-hook 'load-file 'my-mail-load-file)
  (add-hook 'kill-buffer-hook 'my-mail-kill-buffer)
  )

(defun my-mail-load-file ()
  (interactive)
  (auto-fill-mode))

(defun my-mail-kill-buffer ()
  (interactive)
  (progn
	;;  (ispell-buffer) 
	(mark-whole-buffer)
	(untabify 1 (buffer-size))
	(save-buffer)
	)
  )
