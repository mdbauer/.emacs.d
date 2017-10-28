(add-to-list 'load-path "~/.emacs.d/lisp/")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; emacsclient
(server-start)
(setq server-socket-dir "~/.emacs.d/server")
;; to avoid "Buffer foo still has clients; kill it?"
;; https://shreevatsa.wordpress.com/2007/01/06/using-emacsclient/
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; package management
(require 'package)        
(package-initialize)
(unless package-archive-contents ;; refresh package descriptions
  (package-refresh-contents))
(if (string-equal (system-name) "L1MYB00L4")
    ;; behind firewall use mirrored package archive
    ;; https://github.com/d12frosted/elpa-mirror
    (setq package-archives '(("melpa" . "~/.emacs.d/elpa-mirror/melpa/")
                             ("org"   . "~/.emacs.d/elpa-mirror/org/")
                             ("gnu"   . "~/.emacs.d/elpa-mirror/gnu/")))
  ;; otherwise use online archives
  (setq package-archives '(("elpa" . "http://tromey.com/elpa/")
                           ("gnu" . "https://elpa.gnu.org/packages/")
                           ("marmalade" . "https://marmalade-repo.org/packages/")
                           ("melpa" . "https://melpa.org/packages/"))))
(setq package-list '(better-defaults   ;; packages to load
                     recentf
                     ess
                     auctex
                     org-bullets
                     tramp
                     try
                     magit
                     markdown-mode
                     pandoc-mode
                     ))
;; install missing packages
(dolist (package package-list) 
  (unless (package-installed-p package)
    (package-install package)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; various settings

(require 'better-defaults)
;; (require 'hfyview)

(setq inhibit-startup-message t) ;; hide the startup message
;; (global-linum-mode t) ;; enable line numbers globally
(windmove-default-keybindings) ;; to move between windows using shift-key
(global-set-key [f5] 'call-last-kbd-macro)  ;; keyboard macros
(setq next-line-add-newlines t) ;; add new lines when you reach the end of the buffer

;;(set-frame-font "Inconsolata-12" nil t)
(set-frame-font "Consolas-12" nil t)

;; reduce the number of ding warnings
(setq ring-bell-function
      (lambda ()
	(unless (memq this-command
		      '(isearch-abort abort-recursive-edit exit-minibuffer keyboard-quit))
	  (ding))))

;; more informative frame titles
(setq frame-title-format
          '(buffer-file-name
            "%f"
            (dired-directory dired-directory "%b")))

;; dired-x -- https://www.gnu.org/software/emacs/manual/html_node/dired-x/Installation.html#Installation
(add-hook 'dired-load-hook
          (lambda ()
            (load "dired-x")
            ;; Set dired-x global variables here.  For example:
            ;; (setq dired-guess-shell-gnutar "gtar")
            ;; (setq dired-x-hands-off-my-keys nil)
            ))
(add-hook 'dired-mode-hook
          (lambda ()
            ;; Set dired-x buffer-local variables here.  For example:
            ;; (dired-omit-mode 1)
            ))
(autoload 'dired-jump "dired-x"
  "Jump to Dired buffer corresponding to current buffer." t)
(autoload 'dired-jump-other-window "dired-x"
  "Like \\[dired-jump] (dired-jump) but in other window." t)
(define-key global-map "\C-x\C-j" 'dired-jump)
(define-key global-map "\C-x4\C-j" 'dired-jump-other-window)

;; Windows explorer - F12 opens Explorer for current file path
(defun explorer ()
  "Launch the windows explorer in the current directory and selects current file"
  (interactive)
  (w32-shell-execute
   "open"
   "explorer"
   (concat "/e,/select," (convert-standard-filename buffer-file-name))))
(when (string-equal system-type "windows-nt")
  (global-set-key [f12] 'explorer))

;; ido mode
;; see tips at https://www.masteringemacs.org/article/introduction-to-ido-mode
(setq ido-everywhere t) ; better-defaults turns on ido-mode and ido-flex-matching but not this
(setq ido-file-extensions-order '(".tex" ".r" ".org" ".txt" ".py" ".el" ".md")) ; improve sorting of giles in minibuffer
(setq ido-ignore-extensions t) ; so ido can use completion-ignored-extensions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Latex

;; auctex
(require 'tex-site)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil) ; Query for master file.
(when (string-equal system-type "gnu/linux")
  (setq TeX-view-program-selection
        '((output-pdf "Okular"))))
;; (setq TeX-source-specials-mode t)   ; same position in Emacs and DVI viewer
(add-hook 'LaTeX-mode-hook 'flyspell-mode) ;; Use flyspell with latex
 	
(require 'reftex)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
(add-hook 'latex-mode-hook 'turn-on-reftex)   ; with Emacs latex mode
(setq reftex-default-bibliography '("~/Documents/literature.bib"))
(setq reftex-ref-macro-prompt nil)  ; don't ask for reference format
(setq reftex-plug-into-AUCTeX t) ; integrate RefTeX with AUCTeX

;; Beamer -- see http://lists.gnu.org/archive/html/auctex/2006-01/msg00023.html
(eval-after-load "tex"
  '(TeX-add-style-hook "beamer" 'my-beamer-mode))

(setq TeX-region "regionsje")
(defun my-beamer-mode ()
  "My add ons for when in beamer."
  (TeX-PDF-mode 1)                      ;turn on PDF mode.
  ;; Tell reftex to treat \lecture and \frametitle as section commands
  ;; so that C-c = gives you a list of frametitles and you can easily
  ;; navigate around the list of frames.
  ;; If you change reftex-section-level, reftex needs to be reset so that
  ;; reftex-section-regexp is correctly remade.
  (set (make-local-variable 'reftex-section-levels)
       '(("lecture" . 1) ("frametitle" . 2)))
  (reftex-reset-mode)
  ;; add some extra functions.
  (define-key LaTeX-mode-map "\C-cf" 'beamer-template-frame)
  (define-key LaTeX-mode-map "\C-\M-x" 'tex-frame)
)

(defun tex-frame ()
  "Run pdflatex on current frame.
Frame must be declared as an environment."
  (interactive)
  (let (beg)
    (save-excursion
      (search-backward "\\begin{frame}")
      (setq beg (point))
      (forward-char 1)
      (LaTeX-find-matching-end)
      (TeX-pin-region beg (point))
      (letf (( (symbol-function 'TeX-command-query) (lambda (x) "LaTeX")))
        (TeX-command-region))
        )
      ))

(defun beamer-template-frame ()
  "Create a simple template and move point to after \\frametitle."
  (interactive)
  (LaTeX-environment-menu "frame")
  (insert "\\frametitle{}")
  (backward-char 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; others

;; recentf -- https://www.emacswiki.org/emacs/RecentFiles
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; markdown mode
(add-to-list 'auto-mode-alist '("\\.Rmd\\'" . markdown-mode))
;; (add-hook 'markdown-mode-hook
;; 	  (lambda ()
;; 	    (markdown-enable-math)))
(setq markdown-enable-math t)
(setq markdown-command "pandoc --from markdown")
(load "pandoc-mode")
(add-hook 'markdown-mode-hook 'pandoc-mode)

;; org-mode
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cc" 'org-capture)
(setq org-directory "~/Documents/org/")
(setq org-default-notes-file (concat org-directory "notes.org"))
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; theme
(load-theme 'tango-dark t)

;; ESS
;;(require 'ess-site) ;; this takes a while, doesn't seem necessary
(setq inferior-R-args "--no-save ")
(when (string-equal (system-name) "L1MYB00L4")
  (setq inferior-R-program-name "C:/Program Files/R/R-3.4.0/bin/x64/Rterm"))
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

;; magit
(global-set-key (kbd "C-x g") 'magit-status)

;; compile (make) with  C-x C-m C-m  or C-x RET RET
(setq compilation-scroll-output t)
(global-set-key (kbd "C-x C-m") 'compile) 

;; fixing prompts -- https://www.masteringemacs.org/article/disabling-prompts-emacs
(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-nonexistent-file-or-buffer nil)
(setq ido-create-new-buffer 'always)
(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function
        kill-buffer-query-functions))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-command "latex -file-line-error")
 '(org-agenda-files
   (quote
    ("~/Documents/trends/trends.org" "~/Documents/rstar/rstar.org" "~/Documents/org/tm.org")))
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-clock-into-drawer t)
 '(org-file-apps
   (quote
    ((auto-mode . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . default)
     ("\\.pdf\\'" . default)
     ("\\.docx\\'" . default))))
 '(safe-local-variable-values (quote ((TeX-master . t))))
 '(package-selected-packages
   (quote
    (try pandoc-mode org-bullets markdown-mode magit flycheck ess better-defaults auctex))))
;; https://www.reddit.com/r/emacs/comments/53zpv9/how_do_i_get_emacs_to_stop_adding_custom_fields/
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; (find-file "~/Documents/org/tm.org")
