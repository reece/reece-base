(add-hook 'sql-interactive-mode-hook
    (function (lambda ()
        (setq comint-input-ring-file-name "~/.sql_history")
        (setq comint-input-ring-size 100)
        (setq comint-output-filter-functions 'comint-truncate-buffer)
        (setq tab-width 8)
	)))
