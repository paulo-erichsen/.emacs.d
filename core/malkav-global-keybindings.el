;;; malkav-global-keybindings.el --- Emacs Malkav: some useful keybindings.
;;
;;; Commentary:
;; Lots of useful keybindings.
;;
;;; Code:

;; scroll up/down w/ meta-up or meta-down
(global-set-key [M-up]   (lambda () (interactive) (scroll-down 1)))
(global-set-key [M-down] (lambda () (interactive) (scroll-up   1)))

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch)

(provide 'malkav-global-keybindings)
;;; malkav-global-keybindings.el ends here
