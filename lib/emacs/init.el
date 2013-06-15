(setq emacs-home-dir "~/opt/reece-base/lib/emacs")
(add-to-list 'load-path emacs-home-dir)
(add-to-list 'load-path (concat emacs-home-dir "/plugins" ))


(load-library "Rtrinkets")

(autoload 'c-outline "c-outline" "Outline mode for C and Perl" t)
(load-library "shebang")
;(load-library "popchar")

; ==== CUSTOMIZATIONS
;(load-library "init/browse-url")
;(load-library "init/c-mode")
;(load-library "init/dired")
;(load-library "init/elisp-mode")
;(and window-system (not running-xemacs)
;     (load-library "init/font-lock"))
(load-library "init/ess")
(load-library "init/modeline")
(load-library "init/perl-mode")
(load-library "init/javascript-mode")
(load-library "init/marmalade")
(load-library "init/python-mode")
(load-library "init/scala-mode")
(load-library "init/tramp")
;(load-library "init/sh-mode")
;(load-library "init/shell")
;(load-library "init/sql")
;(load-library "init/supercite")

;(load-library "init/bibtex-mode")
;(load-library "init/fortran-mode")
;(load-library "init/my-mail-mode")
;(load-library "init/tex-mode")



;(require 'ido)
;(ido-mode t)
;(setq ido-enable-flex-matching t) ;; enable fuzzy matching

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(add-to-list 'load-path (concat emacs-home-dir "/plugins/auto-complete" ))
(add-to-list 'load-path (concat emacs-home-dir "/plugins/auto-complete/dict" ))
(require 'auto-complete-config)
(ac-config-default)

(add-hook 'python-mode-hook 'auto-complete-mode)
;(add-hook 'python-mode-hook 'jedi:ac-setup)

;(add-hook 'after-init-hook 'global-flycheck-mode)

(require 'yasnippet)



; ==== MUST BE LAST TO LOAD
(load-library "init/keys,vars,misc")

(server-start)

;; (if running-xemacs
;;     (vc-load-vc-hooks)
;;   )
;; (custom-set-faces
;;   ;; custom-set-faces was added by Custom -- don't edit or cut/paste it!
;;   ;; Your init file should contain only one such instance.
;;  '(cperl-hash-face ((((class color) (background light)) (:foreground "Red" :bold t))))
;;  '(info-node ((t (:foreground "red" :bold t))))
;;  '(man-italic ((t (:foreground "red" :bold nil)))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bookmark-default-file "~/opt/reece-base/etc/emacs.bmk")
 '(browse-url-browser-function (quote browse-url-generic))
 '(browse-url-generic-program "xdg-open")
 '(column-number-mode t)
 '(custom-safe-themes (quote ("1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default)))
 '(display-time-mode t)
 '(safe-local-variable-values (quote ((encoding . utf-8))))
 '(scroll-bar-mode (quote right)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 78 :width normal :foundry "unknown" :family "DejaVu Sans Mono"))))
 '(cperl-hash-face ((((class color) (background light)) (:foreground "Red" :bold t))) t)
 '(info-node ((t (:foreground "red" :bold t))))
 '(man-italic ((t (:foreground "red" :bold nil))) t))

