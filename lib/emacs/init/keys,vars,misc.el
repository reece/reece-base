;$Id: keys,vars,misc.el,v 1.15 2007/10/15 20:13:23 rkh Exp $
;;; key and variable customizations
;;; WARNING: byte-compile'ing with xemacs causes some keys to be mis-mapped
;;; in GNU emacs.  b-c with emacs for compatibility.
;===============================================================================

;; ; http://www.emacswiki.org/emacs/CopyAndPaste
;; (global-set-key "\C-w" 'clipboard-kill-region)
;; (global-set-key "\M-w" 'clipboard-kill-ring-save)
;; (global-set-key "\C-y" 'clipboard-yank)

(global-set-key "\C-c\C-b"	'bury-buffer)
(global-set-key "\C-c\C-f"	'ffap)
(global-set-key "\C-c\C-l"	'goto-line)
(global-set-key "\C-c\C-k"	'kill-rectangle)
(global-set-key "\C-c\C-r"	'revert-buffer-noconfirm)
(global-set-key "\C-c\C-y"	'yank-rectangle)
(global-set-key "\C-cs"		'(lambda ()
							   (interactive)
							   (insert (format-time-string "%Y-%m-%d %H:%M Reece Hart <reecehart@gmail.com>"))))
(global-set-key "\C-cS"		'(lambda ()
							   (interactive)
							   (insert (format-time-string "%Y-%m-%d %H:%M Reece Hart <reece.hart@invitae.com>"))))
(global-set-key "\C-c\C-d"	'(lambda ()
							   (interactive)
							   (insert (format-time-string "* %Y-%m-%d %H:%M"))))
(global-set-key "\C-c="		'sort-words-in-region)
(global-set-key "\C-cu."	'browse-url-at-point)
(global-set-key "\C-cub"	'browse-url-of-buffer)
(global-set-key "\C-cuf"	'browse-url-of-file)
(global-set-key "\C-x\C-c"	'delete-frame-or-kill-emacs)
(global-set-key "\C-x\C-e"	'compile)
(global-set-key "\C-x\C-m"	'manual-entry)
(global-set-key "\C-x\C-n"	'next-error)
(global-set-key "\M-#"		'ispell-buffer)
(global-set-key "\M-$"		'ispell-word)
(global-set-key "\M-&"		'ispell-region)
(global-set-key "\M-\t"		'ispell-complete-word)
(global-set-key "\M-~"		'eval-region)
(global-set-key "\M-\C-h"	'backward-kill-word)
(global-set-key "\M-?"      'help-command)
;(global-set-key "\C-h"      'backward-delete-char)

(global-set-key [f2] 		'(lambda ()		; toggle accent mode
							   (interactive)
							   (setq minor-mode-alist
									 (if (iso-accents-mode)
										 (cons " iso" minor-mode-alist)
									   (delete " iso" minor-mode-alist)))
							   (force-mode-line-update)))
(global-set-key [f3] 		'(lambda ()		; toggle line truncation
							   (interactive)
							   (setq truncate-lines (not truncate-lines))
							   (redraw-display)))
(global-set-key [mouse-4] '(lambda () (interactive) (scroll-down 4)))
(global-set-key [mouse-5] '(lambda () (interactive) (scroll-up 4)))

(define-key query-replace-map [return] 'act)
(define-key query-replace-map [?\C-m] 'act)

(if running-xemacs
	;; xemacs only
	(progn
	  (global-set-key [end]       'end-of-buffer) ;xemacs
	  (global-set-key [home]      'beginning-of-buffer) ;xemacs
	  (font-lock-mode 1)
	  )
    ;; emacs only
	(progn
	  (global-set-key [?\C-%]     'query-replace-regexp) 
	  (global-set-key [f1] 		'transient-mark-mode) ; show 1° and 2° selection
	  (global-set-key [f4] 		'popchar-insert-char) ; insert foreign char
	  (global-set-key [f5] 		'citation-insert) ; insert citation
	  (global-set-key [?\C-<]	'(lambda ()	"shrink window by one line"
								   (interactive) (enlarge-window -1)))
	  (global-set-key [?\C->]	'(lambda ()	"enlarge window by one line"
								   (interactive) (enlarge-window 1)))
	  (define-key global-map [M-S-down-mouse-3] 'imenu)
	  ))


;; variable customizations
(setq
 efs-debug-ftp-connection t
 efs-ftp-local-host-regexp ".gene.com"
 efs-ftp-program-args (quote ("-i" "-g" "-v" "-n" "-u"))
 efs-ftp-program-name "/usr/local/tools/bin/ftp"
 efs-generate-anonymous-password "rkh@gene.com"
 efs-local-host-regexp ".gene.com"

 ftp-program "rftp"
 gnuserv-frame (quote gnuserv-visible-frame-function)
 ispell-program-name "aspell"
 ispell-use-ptys-p nil
 load-home-init-file t
 user-mail-address "rkh@tallac.gene.com"

 Man-notify-method 'friendly				; see variable for meaning
 ange-ftp-process-verbose t
 auto-save-default t						; turn on autosaving
 auto-save-interval 0						; keystrokes
 auto-save-timeout 15						; seconds idle
 backup-by-copying-when-linked t
 column-number-mode t
 compilation-window-height 8				; size for split-window compilation
 compile-command "make -k"
 compile-history (list compile-command)		; seed history with common commands
 crypt-freeze-vs-fortran nil
 crypt-compact-vs-C++ nil
 default-fill-column 74
 default-major-mode 'fundamental-mode
 default-tab-width 4
 default-truncate-lines t
 delete-old-versions t
 diary-file "~/notes/diary"					; diary file name
 dired-backup-overwrite 'always
 dired-listing-switches "-alF"
 dired-ls-F-marks-symlinks t
 display-time-24hr-format t
 enable-recursive-minibuffers t
 font-lock-maximum-decoration t
 frame-title-format "emacs: %b"
 indent-tabs-mode nil						; insert spaces to tab stop
 line-number-mode t
 mail-default-headers "Reply-To: Reece Kimball Hart <reece@in-machina.com>\nX-Attribution: Reece\nX-URL: <URL:http://www.in-machina.com/~reece/>\n"
 mail-signature t
 message-autosave-directory "~/tmp"
 message-cite-function 'sc-cite-original
 message-default-headers mail-default-headers
 message-directory "~/mail"
 message-from-style "angles"
 message-indentation-spaces 2
; metamail-switches (and (require 'metamail) (append (list "-B") metamail-switches))
 mouse-yank-at-point t
 next-line-add-newlines nil
 outline-minor-mode-prefix "\C-c\C-o"
 query-replace-highlight t
 resize-minibuffer-mode t
 search-highlight t
 tex-dvi-view-command "xdvi"
 vc-checkout-carefully t
 vc-command-messages t
 vc-make-backup-files t
 vc-mistrust-permissions t
 version-control t							; number backups
 )

(custom-set-faces
 '(info-node ((t (:foreground "red" :bold t))))
 '(cperl-hash-face ((((class color) (background light)) (:foreground "Red" :bold t))))
 '(man-italic ((t (:foreground "red" :bold nil)))))

(let ((lat (getenv "LATITUDE"))
	  (lon (getenv "LONGITUDE")))
  (and (not (null lat)) (setq calendar-latitude (string-to-number lat)))
  (and (not (null lon)) (setq calendar-longitude (string-to-number lon))))

(setq interpreter-mode-alist
	  (append interpreter-mode-alist (list '("make" . makefile-mode ))))

;(if (not window-system)	(menu-bar-mode -1)) ; no menubar in text environment

(set-input-mode nil nil t)					; enable meta key in xwsh windows
											; also, in Xdefaults:
											; XWsh.keyboardType:     xlib
											; XWsh.metaKeyMask:      8
											; XWsh.initSequence:     \233?1h

(fset 'yes-or-no-p 'y-or-n-p)				; perm. replace yes-or-no-p

(put 'erase-buffer 'disabled nil)
(put 'eval-expression 'disabled nil)

;; File buffers are usually editable by root, even if the file permissions
;; specify read-only; the following hook makes buffers read-only by root
;; if permissions are a-w (mask=128+16+2).
(if (= 0 (user-uid))
	(add-hook 'find-file-hooks
	 '(lambda () (if (and (file-exists-p buffer-file-name)
						  (= 0 (logand (file-modes buffer-file-name)
									   (+ 128 16 2))))
					 (toggle-read-only +1)))))

; don't make backup files of pine (~/tmp/pico*) email files
(add-hook 'find-file-hooks
		  '(lambda () (if (string-match "/pico*" buffer-file-name)
						  (setq backup-inhibited t))))

(add-hook 'compilation-mode-hook '(lambda () (setq tab-width 4)))
(add-hook 'Info-mode-hook 'tab-width-8-local)
(add-hook 'mail-send-hook 'ispell-message)	; for mail posts via sendmail
(add-hook 'news-inews-hook 'ispell-message)	; For news posts

(if window-system
	(global-set-key "\C-x\C-c" 'my-exit-from-emacs))

;===============================================================================
