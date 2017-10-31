;;;; my better defaults

(setq inhibit-startup-message t) ;; hide the startup message
;; (global-linum-mode t) ;; enable line numbers globally
(windmove-default-keybindings) ;; to move between windows using shift-key
(global-set-key [f5] 'call-last-kbd-macro)  ;; keyboard macros
(setq next-line-add-newlines t) ;; add new lines when you reach the end of the buffer
(setq-default major-mode 'text-mode) ;; text mode instead of fundamental mode as default
(global-set-key "\C-cw" 'compare-windows) ;; compare text, move point in both windows
(global-set-key "\C-co" 'occur) ;; show all lines in buffer that match regex
(blink-cursor-mode 0)
;; (menu-bar-mode 1)  -- is on by default
(setq tooltip-delay 0.1)  ;; for tooltip-mode -- default is 0.7 seconds
(setq-default fill-column 80)

;; fixing prompts -- https://www.masteringemacs.org/article/disabling-prompts-emacs
(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-nonexistent-file-or-buffer nil)
(setq ido-create-new-buffer 'always)
(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function
        kill-buffer-query-functions))

;; ido mode
(ido-mode t)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t) 
(setq ido-file-extensions-order '(".tex" ".r" ".org" ".txt" ".py" ".el" ".md")) ; improve sorting of giles in minibuffer
(setq ido-ignore-extensions t) ; so ido can use completion-ignored-extensions

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

;; instant access to my init file
(defun find-user-init-file ()
  "Edit the `user-init-file'"
  (interactive)
  (find-file user-init-file))
(global-set-key (kbd "C-c I") 'find-user-init-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; this part from better-defaults package, with some changes

;; (unless (fboundp 'helm-mode)
;;   (ido-mode t)
;;   (setq ido-enable-flex-matching t))

;; (menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(require 'saveplace)
(setq-default save-place t)

(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-z") 'zap-up-to-char)

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

(show-paren-mode 1)  ;; matching parenthesis
(setq-default indent-tabs-mode nil)    ;; don't use tabs but spaces to indent
(setq save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t
      require-final-newline t
      visible-bell t
      load-prefer-newer t
      ediff-window-setup-function 'ediff-setup-windows-plain
      save-place-file (concat user-emacs-directory "places")
      backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))
