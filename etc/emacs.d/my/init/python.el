(add-to-list 'auto-mode-alist '("\\.wsgi$" . python-mode))

; shebang lines (when suffixes aren't used)
(add-to-list 'interpreter-mode-alist '("python2.7" . python-mode))
(add-to-list 'interpreter-mode-alist '("python3"   . python-mode))
(add-to-list 'interpreter-mode-alist '("python3.1" . python-mode))
(add-to-list 'interpreter-mode-alist '("python3.2" . python-mode))
(add-to-list 'interpreter-mode-alist '("python3.3" . python-mode))
(add-to-list 'interpreter-mode-alist '("python3.4" . python-mode))

(require 'python-mode)

(pyvenv-workon "default-2.7")

(elpy-enable)
(elpy-use-ipython)

(eval-after-load "python"
  '(progn
     (define-key python-mode-map "\C-cx" 'jedi-direx:pop-to-buffer)

     ;; make underscore a word delimiter
     (modify-syntax-entry ?_ "_" python-mode-syntax-table)
     )
)


;; (add-hook 'python-mode-hook 'eldoc-mode)
;; (add-hook 'python-mode-hook 'anaconda-mode)
;; (add-hook 'python-mode-hook 'auto-complete-mode

; inspiration:
; https://launchpad.net/python-mode/trunk/6.1.1/+download/python-mode.el-6.1.1.tar.gz

