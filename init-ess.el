;; ESS
(require 'ess-site) ;; this takes a while, doesn't seem necessary
(setq inferior-R-args "--no-save ")
;; (when (string-equal (system-name) "L1MYB00L5")
;;   (setq inferior-R-program-name "C:/Program Files/R/R-3.5.0/bin/x64/Rterm"))
;; (setq inferior-STAT-program-name "C:/Program Files (x86)/Stata14/StataMP-64.exe")
;; (setq inferior-julia-program-name "C:/Users/Michael/AppData/Local/Julia-0.6.0/bin/julia.exe")
;; (setq inferior-julia-args "-L essfix.jl")
; tips from http://emacswiki.org/emacs/EmacsSpeaksStatistics
(setq ess-eval-visibly-p nil) ; otherwise C-c C-r (eval region) takes forever
;; (setq ess-ask-for-ess-directory nil) ;otherwise you are prompted each time you start an interactive R session

;; following R coding standards recommendations
;; http://stat.ethz.ch/CRAN/doc/manuals/R-ints.html#R-coding-standards
(add-hook 'ess-mode-hook
	  (lambda ()
	    (ess-set-style 'C++ 'quiet)
	    (add-hook 'local-write-file-hooks
		      (lambda ()
			(ess-nuke-trailing-whitespace)))))
(setq ess-nuke-trailing-whitespace-p t)
(setq c-default-style "bsd"
      c-basic-offset 4)
