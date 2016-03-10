(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(markdown-command "/usr/bin/pandoc")
 '(opascal-indent-level 2)
 '(pascal-indent-level 2)
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; follow symbolic links to Git-controlled source files
(setq vc-follow-symlinks t)

;; location of game score files
(setq tetris-score-file "~/tmp/tetris-scores")
(setq snake-score-file "~/tmp/snake-scores")

;; Fortune path
(require 'fortune)
(setq fortune-dir "/usr/share/games/fortunes"
      fortune-file "/usr/share/games/fortunes/fortunes")
