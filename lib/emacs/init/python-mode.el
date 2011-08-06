;===============================================================================
;(autoload 'python-mode "python-mode" "" t)
(defun reece-python-mode-setup ()
  (setq
   indent-tabs-mode		nil)
  )
(add-hook 'python-mode-hook 'reece-python-mode-setup)
;===============================================================================
