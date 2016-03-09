;; turn off Indent Tabs mode -> use spaces rather than tabs!!
(setq-default indent-tabs-mode nil)

;; delete white-spaces on save
(add-hook 'before-save-hook 'whitespace-cleanup)

(provide 'malkav-editor)
