; downloaded from:
; https://launchpad.net/python-mode/trunk/6.1.1/+download/python-mode.el-6.1.1.tar.gz

(add-to-list 'auto-mode-alist '("\\.wsgi$" . python-mode))

; shebang lines (when suffixes aren't used)
(add-to-list 'interpreter-mode-alist '("python2.7" . python-mode))
(add-to-list 'interpreter-mode-alist '("python3"   . python-mode))
(add-to-list 'interpreter-mode-alist '("python3.1" . python-mode))
(add-to-list 'interpreter-mode-alist '("python3.2" . python-mode))
(add-to-list 'interpreter-mode-alist '("python3.3" . python-mode))
(add-to-list 'interpreter-mode-alist '("python3.4" . python-mode))

(setq-default
;;    elpy-rpc-backend "jedi"
;;  indent-tabs-mode nil
;;    jedi:complete-on-dot t
;;    jedi:server-args      '("--virtual-env" "/home/reece/.virtualenvs/default-2.7")
;;  tab-width 4
)

(require 'python-mode)

(pyvenv-workon "default-2.7")

(elpy-enable)
(elpy-use-ipython)

;; disabled 2014-09-26 because jedi's not up on my mac yet
;(add-hook 'elpy-mode-hook 'jedi:setup)
;(add-hook 'python-mode-hook 'pyvenv-mode)
;(add-hook 'jedi-mode-hook 'jedi-direx:setup)

(eval-after-load "python"
  '(define-key python-mode-map "\C-cx" 'jedi-direx:pop-to-buffer)

  ;; make underscore a word delimiter
  ;;'(modify-syntax-entry ?_ "_" python-mode-syntax-table)
)





;; (add-hook 'python-mode-hook 'eldoc-mode)

;; (add-hook 'python-mode-hook 'anaconda-mode)
;; (add-hook 'python-mode-hook 'auto-complete-mode)

;; (setq
;;  py-docstring-style "DJANGO"
;;  )
;; 
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
