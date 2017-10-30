;; set load path for custom lisp
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; emacs custom-settings in separate file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

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
  (setq package-archives '(("melpa" . "https://melpa.org/packages/")
			   ("elpa" . "http://tromey.com/elpa/")
                           ("gnu" . "https://elpa.gnu.org/packages/")
                           ("marmalade" . "https://marmalade-repo.org/packages/"))))
(setq package-list '(better-defaults   ;; packages to load
                     recentf
                     ess
                     auctex
                     org-bullets
                     tramp
                     magit
                     markdown-mode
                     pandoc-mode
                     ))
;; install missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(require 'better-defaults)
;; (require 'hfyview)

;; Latex
(load "~/.emacs.d/init-latex.el")

;; ESS
(load "~/.emacs.d/init-ess.el")

;; For Windows
(when (string-equal system-type "windows-nt")
  (load "~/.emacs.d/init-windows.el"))

(setq inhibit-startup-message t) ;; hide the startup message
;; (global-linum-mode t) ;; enable line numbers globally
(windmove-default-keybindings) ;; to move between windows using shift-key
(global-set-key [f5] 'call-last-kbd-macro)  ;; keyboard macros
(setq next-line-add-newlines t) ;; add new lines when you reach the end of the buffer

(if (string-equal system-type "windows-nt")
    (set-frame-font "Consolas-12" nil t)
  (set-frame-font "Inconsolata-12" nil t))

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

;; ido mode
;; see tips at https://www.masteringemacs.org/article/introduction-to-ido-mode
(setq ido-everywhere t) ; better-defaults turns on ido-mode and ido-flex-matching but not this
(setq ido-file-extensions-order '(".tex" ".r" ".org" ".txt" ".py" ".el" ".md")) ; improve sorting of giles in minibuffer
(setq ido-ignore-extensions t) ; so ido can use completion-ignored-extensions

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
(load "~/.emacs.d/init-org.el")

;; theme
(load-theme 'tango-dark t)

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

;; Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))
(setq server-socket-dir "~/.emacs.d/server")
;; to avoid "Buffer foo still has clients; kill it?"
;; https://shreevatsa.wordpress.com/2007/01/06/using-emacsclient/
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)

;; Run at full power please
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; does this speed up finding files?
(remove-hook 'find-file-hooks 'vc-find-file-hook)

(find-file "~/Documents/org/tm.org")
