;;; Latex
;; auctex
(require 'tex-site)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil) ; Query for master file.

;; (setq TeX-source-specials-mode t)   ; same position in Emacs and DVI viewer
(add-hook 'LaTeX-mode-hook 'flyspell-mode) ;; Use flyspell with latex
 	
(require 'reftex)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
(add-hook 'latex-mode-hook 'turn-on-reftex)   ; with Emacs latex mode
(setq reftex-default-bibliography '("~/texmf/bibtex/bib/mybibs/literature.bib"))
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
  "Create a simple frame template and move point to heading."
  (interactive)
  (LaTeX-environment-menu "frame")
  ;; (insert "\\frametitle{}")
  ;; (backward-char 1))
  (backward-char 3)
  (insert "{}")
  (backward-char 1))

;; forward/inverse search
;; http://william.famille-blum.org/blog/static.php?page=static081010-000413
;; http://www.barik.net/archive/2012/07/18/154432/
;; https://stackoverflow.com/questions/14448606/sync-emacs-auctex-with-sumatra-pdf
;; http://www.kevindemarco.com/2013/04/24/emacs-auctex-synctex-okular-on-ubuntu-12-04/
(setq TeX-source-correlate-method 'synctex)
(setq TeX-source-correlate-mode t)
(setq TeX-source-correlate-start-server t)

(if (string-equal system-type "windows-nt")
    (setq TeX-view-program-list
          ;; (if (string-equal (system-name) "UN-WKS-009761")
          ;;     '(("Sumatra PDF" ("\"C:/Program Files/SumatraPDF/SumatraPDF.exe\" -reuse-instance"
          ;;                       (mode-io-correlate " -forward-search %b %n ") " %o")))
          ;;   '(("Sumatra PDF" ("\"C:/Program Files (x86)/SumatraPDF/SumatraPDF.exe\" -reuse-instance"
          ;;                     (mode-io-correlate " -forward-search %b %n ") " %o")))))
          '(("Sumatra PDF" ("\"C:/Program Files/SumatraPDF/SumatraPDF.exe\" -reuse-instance"
                            (mode-io-correlate " -forward-search %b %n ") " %o"))))
  (setq TeX-view-program-list (quote (("Okular" "okular --unique %o#src:%n%b")))))

;; (string-equal (system-name) "UN-WKS-009761") "C:/Dropbox")

(eval-after-load 'tex
  '(progn
     (assq-delete-all 'output-pdf TeX-view-program-selection)
     (if (string-equal system-type "windows-nt")
         (add-to-list 'TeX-view-program-selection '(output-pdf "Sumatra PDF"))
       (add-to-list 'TeX-view-program-selection '(output-pdf "Okular")))))
