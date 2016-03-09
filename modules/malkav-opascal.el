;; associate pascal files as opascal-mode
(add-to-list 'auto-mode-alist '("\\.pas$" . opascal-mode))
(let* ((pascal-files '(".pas" ".pp" ".inc" ".lpr" ".dpr"))
       (pascal-regexp (concat (regexp-opt pascal-files t) "\\'")))
  (add-to-list 'auto-mode-alist (cons pascal-regexp 'opascal-mode)))

;; don't escape backslashes in pascal ~ http://emacs.stackexchange.com/a/20639/11226
(add-hook 'opascal-mode-hook (lambda ()
                               (modify-syntax-entry ?\\ ".")))
(provide 'malkav-opascal)
