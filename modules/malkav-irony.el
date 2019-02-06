;;; malkav-irony.el --- Emacs Malkav: irony-mode configuration
;;; Commentary:
;; add flycheck-irony and company-irony

;;; Code:

(malkav-require-package 'company-irony)
(malkav-require-package 'company-irony-c-headers)
(malkav-require-package 'flycheck-irony)
(malkav-require-package 'irony-eldoc)

;; setup irony
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)
(add-hook 'irony-mode-hook #'irony-eldoc)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; setup flycheck and company
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))

;; irony-c-headers
(require 'company-irony-c-headers)
;; Load with `irony-mode` as a grouped backend
(eval-after-load 'company
  '(add-to-list
    'company-backends '(company-irony-c-headers company-irony)))

(provide 'malkav-irony)
;;; malkav-irony.el ends here
