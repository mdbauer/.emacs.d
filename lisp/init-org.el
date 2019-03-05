;; org-mode
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cc" 'org-capture)
(setq
 org-directory org-path
 org-default-notes-file (concat org-directory "notes.org")
 org-agenda-files
 (append '("~/Documents/trends/trends.org" "~/Documents/mpu/mpu.org" "~/Documents/fed-info/fed-info.org") (list tm-path))
 org-agenda-skip-deadline-if-done t
 org-agenda-skip-scheduled-if-done t
 org-clock-into-drawer t
 org-file-apps
 (quote
  ((auto-mode . emacs)
   ("\\.mm\\'" . default)
   ("\\.x?html?\\'" . default)
   ("\\.pdf\\'" . default)
   ("\\.docx\\'" . default))))

(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
