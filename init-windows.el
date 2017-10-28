;; Windows explorer - F12 opens Explorer for current file path
(defun explorer ()
  "Launch the windows explorer in the current directory and selects current file"
  (interactive)
  (w32-shell-execute
   "open"
   "explorer"
   (concat "/e,/select," (convert-standard-filename buffer-file-name))))
(global-set-key [f12] 'explorer)

;; shell
;; from https://www.masteringemacs.org/article/running-shells-in-emacs-overview
;; shortcut: "C:\Program Files\Git\git-bash.exe" --cd-to-home
(setq explicit-shell-file-name "C:/Program Files/Git/git-bash.exe")
(setq shell-file-name "bash")
(setq explicit-bash.exe-args '("--noediting" "--login" "-i"))
(setenv "SHELL" shell-file-name)
(add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)
