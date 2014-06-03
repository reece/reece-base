; -*-emacs-lisp-*-

(setq user-emacs-dir "~/.emacs.d")
(add-to-list 'load-path user-emacs-dir)

(load-library "init/package-install")

(require 'tramp)

(require 'auto-complete)
(require 'auto-complete-config)
(setq ac-dictionary-files (list (concat user-emacs-directory ".dict")))
(ac-config-default)

(load-library "pkgs/Rtrinkets")
(load-library "pkgs/shebang")

(load-library "init/python-mode")

(load-library "init/keys,vars,misc")	; should be decomposed

(yas-global-mode 1)
;(yas-load-directory)

(server-start)


;; ############################################################################

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#ad7fa8" "#8cc4ff" "#eeeeec"])
 '(background-color "#002b36")
 '(background-mode dark)
 '(bookmark-default-file "~/.emacs.d/emacs.bmk")
 '(column-number-mode t)
 '(cursor-color "#839496")
 '(custom-enabled-themes (quote (adwaita)))
 '(custom-safe-themes (quote ("1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default)))
 '(foreground-color "#839496")
 '(inhibit-startup-screen t)
 '(package-archives (quote (("gnu" . "http://elpa.gnu.org/packages/") ("melpa" . "http://melpa.milkbox.net/packages/") ("marmalade" . "http://marmalade-repo.org/packages/"))))
 '(py-underscore-word-syntax-p nil)
 '(safe-local-variable-values (quote ((encoding . utf-8)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "unknown" :slant normal :weight normal :height 78 :width normal)))))
