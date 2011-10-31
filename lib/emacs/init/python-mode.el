;===============================================================================
; http://www.emacswiki.org/emacs/PythonProgrammingInEmacs
; https://bitbucket.org/jonwaltman/pydoc-info/
; http://stackoverflow.com/questions/1054903/how-do-you-get-python-documentation-in-texinfo-info-format

;(autoload 'python-mode "python-mode" "" t)

(defun reece-python-mode-setup ()
  ; (local-set-key (kbd "C-h f") 'hohe2-lookup-pydoc))
  ; (local-set-key (kbd "C-h f") 'nikokrock_pydoc))
  (setq
   indent-tabs-mode		nil)
  )
(add-hook 'python-mode-hook 'reece-python-mode-setup)

(add-to-list 'interpreter-mode-alist '("python2.7" . python-mode))
(add-to-list 'interpreter-mode-alist '("python3" . python-mode))
(add-to-list 'interpreter-mode-alist '("python3.1" . python-mode))
(add-to-list 'interpreter-mode-alist '("python3.2" . python-mode))
(add-to-list 'interpreter-mode-alist '("python3.3" . python-mode))

(add-to-list 'load-path (concat emacs-home-dir "/pydoc-info"))
(require 'pydoc-info)

;===============================================================================



;; ;;----pydoc lookup----
;; (defun hohe2-lookup-pydoc ()
;;   (interactive)
;;   (let ((curpoint (point)) (prepoint) (postpoint) (cmd))
;;     (save-excursion
;;       (beginning-of-line)
;;       (setq prepoint (buffer-substring (point) curpoint)))
;;     (save-excursion
;;       (end-of-line)
;;       (setq postpoint (buffer-substring (point) curpoint)))
;;     (if (string-match "[_a-z][_\\.0-9a-z]*$" prepoint)
;;         (setq cmd (substring prepoint (match-beginning 0) (match-end 0))))
;;     (if (string-match "^[_0-9a-z]*" postpoint)
;;         (setq cmd (concat cmd (substring postpoint (match-beginning 0) (match-end 0)))))
;;     (if (string= cmd "") nil
;;       (let ((max-mini-window-height 0))
;;         (shell-command (concat "pydoc " cmd))))))
;; 
;; (defun nikokrock_pydoc (&optional arg)
;;   (interactive)
;;   (when (not (stringp arg))
;;     (setq arg (thing-at-point 'word)))
;; 
;;   (setq cmd (concat "pydoc " arg))
;;   (ad-activate-regexp "auto-compile-yes-or-no-p-always-yes")
;;   (shell-command cmd)
;;   (setq pydoc-buf (get-buffer "*Shell Command Output*"))
;;   (switch-to-buffer-other-window pydoc-buf)
;;   (python-mode)
;;   (ad-deactivate-regexp "auto-compile-yes-or-no-p-always-yes")
;; )
