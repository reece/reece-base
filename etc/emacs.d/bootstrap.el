; bootstrap -- install packages for new installation
; last tested with emacs 24.2.1 on Ubuntu 13.04

; prerequisites:
; * virtualenv, activated, with jedi and epc installed, emacs started in ve.
;   E.g., 
;   virtualenv ~/virtualenvs/primary
;   source ~/virtualenvs/primary/bin/activate
;   pip install jedi epc
;   emacs &

(custom-set-variables
 '(custom-enabled-themes (quote (deeper-blue)))
 '(package-archives (quote (("gnu" . "http://elpa.gnu.org/packages/") ("marmalade" . "http://marmalade-repo.org/packages/")))))
(customize-save-customized)

(package-initialize)
(package-refresh-contents)

(package-install 'yasnippet)
(package-install 'coffee-mode)
(package-install 'csv-mode)
(package-install 'jedi)
