(defun explorer ()
  "Launch windows explorer in current directory and, if possible, select current file"
  (interactive)
  (w32-shell-execute
   "open"
   "explorer"
   (if buffer-file-name
       (concat "/e,/select," (convert-standard-filename buffer-file-name))
     (convert-standard-filename default-directory))))
(global-set-key [f12] 'explorer)

;; shell
;; from https://www.masteringemacs.org/article/running-shells-in-emacs-overview
;; shortcut: "C:\Program Files\Git\git-bash.exe" --cd-to-home

;; (setq explicit-shell-file-name "C:/Program Files/Git/git-bash.exe")
;; (setq explicit-bash.exe-args '("--login" "-i"))

;; (setq shell-file-name explicit-shell-file-name)
;; (setenv "SHELL" shell-file-name)
;; (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)

;; (defun git-bash () (interactive)
;;   (let ((explicit-shell-file-name "C:/Program Files/Git/git-bash.exe"))
;;     (call-interactively 'shell)))
