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
