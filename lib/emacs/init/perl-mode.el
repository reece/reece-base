;$Id: perl-mode.el,v 1.4 2007/10/15 18:47:49 rkh Exp $
;;; perl mode
;===============================================================================

(autoload 'cperl-mode "cperl-mode" "alternate mode for editing Perl programs" t)

(setq										
 ;; for some reason these must be set globally
 cperl-font-lock		t
 cperl-comment-column	44
 cperl-indent-parens-as-block   t
 )

(defun Reece-perl-mode-setup ()
  (local-set-key "\C-c\C-d"   'cperl-perldoc)
  (c-outline)
  )
(add-hook 'cperl-mode-hook 'Reece-perl-mode-setup)

;; Use cperl-mode instead of the default perl-mode
(add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\|al\\)\\'" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))

;===============================================================================
