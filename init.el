;;; .emacs.d/init.el -- the `user-init-file'

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(opascal-indent-level 2) ;; set pascal to indent 2 spaces (default is 3)
 '(pascal-indent-level 2) ;; set pascal to indent 2 spaces (default is 3)
 '(markdown-command "/usr/bin/pandoc") ;; make markdown-mode use pandoc
 '(inhibit-startup-screen t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; load some lisp scripts
(add-to-list 'load-path "~/.emacs.d/lisp/")
(load-file "~/.emacs.d/lisp/dired_open.el")

;; scroll up/down w/ meta-up or meta-down
(global-set-key [M-up]   (lambda () (interactive) (scroll-down 1)))
(global-set-key [M-down] (lambda () (interactive) (scroll-up   1)))

;; turn off Indent Tabs mode -> use spaces rather than tabs!!
(setq-default indent-tabs-mode nil)

;; delete white-spaces on save
(add-hook 'before-save-hook 'whitespace-cleanup)

;; uncomment the following to display the line numbers and column numbers
;; (global-linum-mode) ;; display line numbers
(column-number-mode) ;; display column numbers

;; Game Scores
;; http://mewbies.com/emacs_basics_and_miscellaneous_trouble_shooting.htm
;; http://mewbies.com/acute_terminal_fun_06_amusements_and_games_on_the_terminal.htm#emacs
(setq tetris-score-file
      "~/tmp/tetris-scores")
(setq snake-score-file
      "~/tmp/snake-scores")

;; Fortune path
(require 'fortune)
(setq fortune-dir "/usr/share/games/fortunes"
      fortune-file "/usr/share/games/fortunes/fortunes")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Project specific configs ;;;;;;;;;;;;;;;;;

;; associate pascal files as opascal-mode
(add-to-list 'auto-mode-alist '("\\.pas$" . opascal-mode))
(let* ((pascal-files '(".pas" ".pp" ".inc" ".lpr" ".dpr"))
       (pascal-regexp (concat (regexp-opt pascal-files t) "\\'")))
  (add-to-list 'auto-mode-alist (cons pascal-regexp 'opascal-mode)))

;; don't escape backslashes in pascal ~ http://emacs.stackexchange.com/a/20639/11226
(add-hook 'opascal-mode-hook
          (modify-syntax-entry ?\\ "."))

;; action script is not really javascript, but this kinda works for reading
(add-to-list 'auto-mode-alist '("\\.as$" . javascript-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MELPA packages ;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

;; http://emacs.stackexchange.com/a/5888/11226
(package-initialize) ;; this loads auto-complete into the path from MELPA
(setq package-enable-at-startup nil) ;; don't initialize the package again `after-init-hook`

;; auto-complete
(require 'auto-complete-config)
(ac-config-default)
(add-to-list 'ac-dictionary-directories "~/.dict/") ;; add custom dictionary folder
(add-to-list 'ac-modes 'opascal-mode) ;; enable AC automatically for specific modes

;; TODO:
;; - remove the elpa folder from the repository
;; - add code to auto-install/refresh some packages. (auto-complete, lua-mode, markdown-mode, rubocop)
;;   some examples: (search for package-initialize, auto-complete, and use-package, require-package, company)
;;     https://github.com/bbatsov/prelude/blob/master/init.el -> https://github.com/bbatsov/prelude/blob/master/core/prelude-packages.el
;;     https://github.com/purcell/emacs.d/blob/master/init.el
;;     https://github.com/magnars/.emacs.d/blob/master/init.el
;;     https://github.com/skeeto/.emacs.d/blob/master/init.el
;;     http://stackoverflow.com/a/31080940/2261947
;;     https://github.com/jwiegley/use-package/issues/219
;;     https://github.com/thomasf/dotfiles-thomasf-emacs/blob/master/emacs.d/init.el
;;     https://github.com/jimeh/.emacs.d/blob/master/core/siren-packages.el
;;     https://github.com/catatsuy/dot.emacs.d/blob/master/init.el
