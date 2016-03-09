;; scroll up/down w/ meta-up or meta-down
(global-set-key [M-up]   (lambda () (interactive) (scroll-down 1)))
(global-set-key [M-down] (lambda () (interactive) (scroll-up   1)))

(provide 'malkav-global-keybindings)
