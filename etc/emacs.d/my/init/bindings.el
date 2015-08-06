; key and mouse bindings

(global-set-key "\C-c\C-r"      'revert-buffer-noconfirm)
(global-set-key "\C-x\C-c"      'delete-frame-or-kill-emacs)
(global-set-key "\C-x\C-m"      'manual-entry)
(global-set-key "\M-~"          'eval-region)
;; (global-set-key "\C-c="         'sort-words-in-region)
;; (global-set-key "\C-c\C-b"      'bury-buffer)
;; (global-set-key "\C-c\C-f"      'ffap)
;; (global-set-key "\C-c\C-k"      'kill-rectangle)
;; (global-set-key "\C-c\C-l"      'goto-line)
;; (global-set-key "\C-c\C-y"      'yank-rectangle)
;; (global-set-key "\C-cu."        'browse-url-at-point)
;; (global-set-key "\C-cub"        'browse-url-of-buffer)
;; (global-set-key "\C-cuf"        'browse-url-of-file)
;; (global-set-key "\C-x\C-c"      'my-exit-from-emacs))
;; (global-set-key "\C-x\C-e"      'compile)
;; (global-set-key "\C-x\C-n"      'next-error)
;; (global-set-key "\M-#"          'ispell-buffer)
;; (global-set-key "\M-&"          'ispell-region)
;; (global-set-key "\M-?"          'help-command)
;; (global-set-key "\M-\C-h"       'backward-kill-word)
;; (global-set-key "\M-\t"         'ispell-complete-word)


;; (global-set-key "\C-cs"              '(lambda ()
;;                                         (interactive)
;;                                         (insert (format-time-string "%Y-%m-%d %H:%M Reece Hart <reecehart@gmail.com>"))))
;; (global-set-key "\C-cS"              '(lambda ()
;;                                         (interactive)
;;                                         (insert (format-time-string "%Y-%m-%d %H:%M Reece Hart <reece.hart@invitae.com>"))))
;; (global-set-key "\C-c\C-d"   '(lambda ()
;;                                         (interactive)
;;                                         (insert (format-time-string "* %Y-%m-%d %H:%M"))))
;; 
;; (global-set-key [f2]                 '(lambda ()             ; toggle accent mode
;;                                         (interactive)
;;                                         (setq minor-mode-alist
;;                                               (if (iso-accents-mode)
;;                                                      (cons " iso" minor-mode-alist)
;;                                                      (delete " iso" minor-mode-alist)))
;;                                         (force-mode-line-update)))
(global-set-key [\M-f3]                 '(lambda ()             ; toggle line truncation
                                        (interactive)
                                        (setq truncate-lines (not truncate-lines))
                                        (redraw-display)))


; mouse bindings

(defun change-font-height (delta)
  (set-face-attribute 'default 
                                          (selected-frame)
                                          :height (+ (face-attribute 'default :height) delta)))
(global-set-key [C-mouse-4] '(lambda () (interactive) (change-font-height +4)))
(global-set-key [C-mouse-5] '(lambda () (interactive) (change-font-height -4)))
