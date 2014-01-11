; downloaded from:
; https://launchpad.net/python-mode/trunk/6.1.1/+download/python-mode.el-6.1.1.tar.gz

(setq
 py-docstring-style "DJANGO"
 py-install-directory (concat user-emacs-dir "/pkgs/python-mode.el-6.1.1/")
 )
(add-to-list 'load-path py-install-directory)
(require 'python-mode)

; shebang lines (when suffixes aren't used)
(add-to-list 'interpreter-mode-alist '("python2.7" . python-mode))
(add-to-list 'interpreter-mode-alist '("python3"   . python-mode))
(add-to-list 'interpreter-mode-alist '("python3.1" . python-mode))
(add-to-list 'interpreter-mode-alist '("python3.2" . python-mode))
(add-to-list 'interpreter-mode-alist '("python3.3" . python-mode))

; jedi
(autoload 'jedi:setup "jedi" nil t)
(setq
 jedi:complete-on-dot t
 jedi:server-args     '("--virtual-env" "/home/reece/.virtualenvs/default-2.7")
)
(add-hook 'python-mode-hook 'jedi:setup)

(add-hook 'python-mode-hook 'auto-complete-mode)

;; ;; pyflakes flymake integration
;; ;; http://stackoverflow.com/a/1257306/347942
;; (when (load "flymake" t)
;;   (defun flymake-pyflakes-init ()
;;     (let* ((temp-file (flymake-init-create-temp-buffer-copy
;;                        'flymake-create-temp-inplace))
;;            (local-file (file-relative-name
;;                         temp-file
;;                         (file-name-directory buffer-file-name))))
;;       (list "pycheckers" (list local-file))))
;;   (add-to-list 'flymake-allowed-file-name-masks
;;                '("\\.py\\'" flymake-pyflakes-init)))
;; (add-hook 'python-mode-hook 'flymake-mode)
