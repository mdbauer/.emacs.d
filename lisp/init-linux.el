;; open links in Chrome
;; (for some reason it otherwise opens in Firefox even if Chrome is default browser)
;; solution from: https://stackoverflow.com/questions/4506249/how-to-make-emacs-org-mode-open-links-to-sites-in-google-chrome
;; (setq browse-url-browser-function 'browse-url-generic
;;       browse-url-generic-program "google-chrome")
;; set specific browser to open links
(setq browse-url-browser-function 'browse-url-firefox)
