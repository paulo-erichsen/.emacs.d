;;; custom.el --- custom variables

;;; Commentary:

;;; Code:
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "316d29f8cd6ca980bf2e3f1c44d3a64c1a20ac5f825a167f76e5c619b4e92ff4" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "38ba6a938d67a452aeb1dada9d7cdeca4d9f18114e9fc8ed2b972573138d4664" "71ecffba18621354a1be303687f33b84788e13f40141580fa81e7840752d31bf" default)))
 '(global-font-lock-mode t)
 '(global-git-commit-mode)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(markdown-command "/usr/bin/pandoc")
 '(opascal-indent-level 2)
 '(pascal-indent-level 2)
 '(sort-fold-case t t)
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; about (indent-tabs-mode nil)
;; turn off Indent Tabs mode -> use spaces rather than tabs!!
;; toggle while emacs is opened: M-x customize-group -> indent -> toggle -> apply

;; follow symbolic links to Git-controlled source files
(setq vc-follow-symlinks t)

;; location of game score files
(setq tetris-score-file "~/tmp/tetris-scores")
(setq snake-score-file "~/tmp/snake-scores")

;; Fortune path
(require 'fortune)
(setq fortune-dir "/usr/share/games/fortunes"
      fortune-file "/usr/share/games/fortunes/fortunes")

(provide 'custom)
;;; custom.el ends here
