;$Id: c-mode.el,v 1.4 2007/10/15 18:47:49 rkh Exp $
;;; cc-mode
;===============================================================================
;(add-to-load-path (concat emacs-home-dir "/cc-mode"))

; undo old c-mode and c++-mode bindings
(fmakunbound 'c-mode)
(makunbound 'c-mode-map)
(fmakunbound 'c++-mode)
(makunbound 'c++-mode-map)
(makunbound 'c-style-alist)

;; load on demand
(autoload 'c++-mode  "cc-mode" "C++ Editing Mode" t)
(autoload 'c-mode    "cc-mode" "C Editing Mode" t)
(autoload 'objc-mode "cc-mode" "Objective-C Editing Mode" t)

(defconst reece-c-style
  '((c-tab-always-indent			. t)
	(c-basic-offset					. 2)
	(c-comment-only-line-offset		. 0)
	(c-echo-syntactic-information-p . t)
	(c-hanging-braces-alist			. ((substatement-open before after)
									   (brace-list-open)
									   (block-open before)))
	(c-offsets-alist
	 (access-label					. 0)
	 (arglist-close					. c-lineup-arglist)
	 (block-open					. 0)
	 (brace-list-intro				. 0)
	 (brace-list-open				. +)
	 (cpp-macro						. 0)
	 (class-close					. +)
	 (class-open					. +)
	 (defun-open					. +)
	 (inclass           		    . +)
	 (inline-open           	    . +)
	 (knr-argdecl-intro				. +)
	 (label							. -)
	 (defun-block-intro				. 0)
	 (statement-block-intro			. 0)
	 (statement-case-open			. +)
	 (statement-cont				. +)
	 (substatement-open				. +)
	 (topmost-intro					. -)
	 )
	)
  "C/C++ style Reece Kimball Hart <reece@in-machina.com>"
  )

(defun reece-c-mode-setup ()
  ;; Customizations for both c-mode and c++-mode
  ;; set up for my perferred indentation style, but  only do it once
  (c-add-style "Reece's Style <reece@in-machina.com>" reece-c-style t)

  ;; offset customizations not in my-c-style
  (c-set-offset 'member-init-intro '++)

  ;; other customizations
  (set-variable 'comment-column 44)
  (set-variable 'c-auto-newline t)
  (set-variable 'c-brace-offset 4)
  (set-variable 'c-continued-brace-offset -4)
  (set-variable 'c-continued-statement-offset 4)
  (set-variable 'c-indent-level 0)

  (setq
   tab-width 4
   indent-tabs-mode nil	;; use spaces are used instead of tabs
   ffap-c-path (append '("include" "../include"
						 "/usr/local/pbs/include")
                       (ffap-all-subdirs "/usr/local/include" 2)
                       (ffap-all-subdirs "/usr/include" 2)
                       )
   )

  (c-toggle-auto-hungry-state -1)

  (define-key c-mode-map "\C-m" 'newline-and-indent)
;  (define-key c-mode-map [?\C-c-i] 'exclude-region)

  (c-outline)
  )
;(add-hook 'c-mode-common-hook 'reece-c-mode-setup)
;===============================================================================
