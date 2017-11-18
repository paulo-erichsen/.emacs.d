;;; malkav-clang.el --- Emacs Malkav: add clang-format to emacs

;;; Commentary:

;; Add some basic keybinds to use with clang-format

;;; Code:

(require 'malkav-programming)

(malkav-require-packages '(clang-format))

(require 'clang-format)

(global-set-key (kbd "C-c i") 'clang-format-region)
(global-set-key (kbd "C-c u") 'clang-format-buffer)

;; auto-format when saving c++ code
(add-hook 'c++-mode-hook
    (lambda()
        (add-hook 'before-save-hook 'clang-format-buffer)))

(provide 'malkav-clang)
;;; malkav-clang.el ends here
