;;; custom.el --- custom variables

;;; Commentary:

;;; Code:
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '(default))
 '(global-font-lock-mode t)
 '(global-git-commit-mode t)
 '(inhibit-startup-screen t)
 '(markdown-command "/usr/bin/pandoc")
 '(opascal-indent-level 2)
 '(package-selected-packages '(magit git-modes flycheck))
 '(pascal-indent-level 2)
 '(read-file-name-completion-ignore-case nil)
 '(safe-local-variable-values
   '((eval c-set-offset 'arglist-close 0)
     (eval c-set-offset 'arglist-intro '++)
     (eval c-set-offset 'case-label 0)
     (eval c-set-offset 'statement-case-open 0)
     (eval c-set-offset 'substatement-open 0)
     (whitespace-newline . t)
     (whitespace-style face trailing lines-tail space-before-tab indentation empty)
     (whitespace-action warn-read-only auto-cleanup)
     (flycheck-gcc-language-standard . c++11)
     (flycheck-gcc-language-standard . "c++11")
     (flycheck-clang-language-standard . "c++11")
     (flycheck-gcc-language-standard . "c++14")
     (flycheck-clang-language-standard . "c++14")))
 '(sort-fold-case t t))
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
