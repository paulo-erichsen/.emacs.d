;; associate pascal files as opascal-mode
(add-to-list 'auto-mode-alist '("\\.pas$" . opascal-mode))
(let* ((pascal-files '(".pas" ".pp" ".inc" ".lpr" ".dpr"))
       (pascal-regexp (concat (regexp-opt pascal-files t) "\\'")))
  (add-to-list 'auto-mode-alist (cons pascal-regexp 'opascal-mode)))

(add-hook 'opascal-mode-hook
          (lambda ()
            ;; don't escape backslashes in pascal ~ http://emacs.stackexchange.com/a/20639/11226
            (modify-syntax-entry ?\\ ".")
            ;; make it highlight keywords case insensitive
            (setq font-lock-defaults '(opascal-font-lock-keywords nil t nil nil))
            )
          )

;; (defun malkav-opascal-mode-defaults ()
;;   ;; don't escape backslashes in pascal ~ http://emacs.stackexchange.com/a/20639/11226
;;   (modify-syntax-entry ?\\ ".")
;;   ;; make it highlight keywords case insensitive
;;   (setq font-lock-defaults '(opascal-font-lock-keywords nil t nil nil)))

;; (add-hook 'opascal-mode-hook 'malkav-opascal-mode-defaults)

(provide 'malkav-opascal)
