;; Michael's Emacs initialization
;; https://github.com/mdbauer/emacs.d.git

;; set load path for custom lisp
(add-to-list 'load-path "~/.emacs.d/lisp/")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; package management
;; MELPA documenation: https://melpa.org/#/getting-started
(require 'package)
(if (member (system-name) (list "L1MYB00L5" "L1MYB00L4" "l1uerp21.sf.frb.org"))
    ;; behind firewall use: https://github.com/d12frosted/elpa-mirror
    (setq package-archives '(("melpa" . "~/.emacs.d/elpa-mirror/melpa/")
			     ("org"   . "~/.emacs.d/elpa-mirror/org/")
			     ("gnu"   . "~/.emacs.d/elpa-mirror/gnu/")))
  ;; otherwise use online archives
  (if (string-equal system-type "windows-nt")
      (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			       ("marmalade" . "http://marmalade-repo.org/packages/")
			       ("melpa" . "http://melpa.org/packages/")))
    (setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			     ("marmalade" . "https://marmalade-repo.org/packages/")
			     ("melpa" . "https://melpa.org/packages/")))))

;; packages to install
(setq package-list '(ess
                     auctex
                     org-bullets
		     flycheck
                     tramp
                     magit
                     markdown-mode
                     pandoc-mode
                     polymode
                     ))

(package-initialize)
;; refresh package descriptions
(unless package-archive-contents
  (package-refresh-contents))

;; install missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; my version of better-defaults
(load "~/.emacs.d/init-better-defaults.el")

;; Latex
(load "~/.emacs.d/init-latex.el")

;; ESS
(load "~/.emacs.d/init-ess.el")

;; Dired
(load "~/.emacs.d/init-dired.el")

;; Windows/Linux specific configuration
(if (string-equal system-type "windows-nt")
    (load "~/.emacs.d/init-windows.el")
  (load "~/.emacs.d/init-linux.el"))

(load "toggle-window-split.el")

;; ;; nice console fonts
;; (if (string-equal system-type "windows-nt")
;;     (set-frame-font "Consolas-12" nil t)    ;; Windows
;;   (set-frame-font "Inconsolata-12" nil t))  ;; Linux

;; python
(setq python-shell-completion-native-enable nil)

;; recentf -- https://www.emacswiki.org/emacs/RecentFiles
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; markdown mode
;; (add-to-list 'auto-mode-alist '("\\.Rmd\\'" . markdown-mode))
(setq markdown-enable-math t)
(setq markdown-command "pandoc --from markdown")
(load "pandoc-mode")
(add-hook 'markdown-mode-hook 'pandoc-mode)

;;; polymode
;; MARKDOWN
(add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode))
;; R modes
(add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))

;; org-mode
(load "~/.emacs.d/init-org.el")

;; theme
(load-theme 'tango-dark t)

;; magit
(global-set-key (kbd "C-x g") 'magit-status)

;; compile (make) with  C-x C-m C-m  or C-x RET RET
(setq compilation-scroll-output t)
(global-set-key (kbd "C-x C-m") 'compile)

;; Emacs server
;; (require 'server)
;; (unless (server-running-p)
(server-start)
;; (setq server-socket-dir "~/.emacs.d/server")
;; to avoid "Buffer foo still has clients; kill it?"
;; https://shreevatsa.wordpress.com/2007/01/06/using-emacsclient/
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)

;; don't ask for confirmation of these commands
(put 'downcase-region 'disabled nil)  ;; C-x C-l
(put 'upcase-region 'disabled nil)    ;; C-x C-u
(put 'narrow-to-region 'disabled nil) ;; C-x n n

;; emacs custom-settings in separate file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; open my time-management org-file if it exists
(if (file-exists-p "~/Documents/org/tm.org")
    (setq initial-buffer-choice "~/Documents/org/tm.org"))
(put 'set-goal-column 'disabled nil)
