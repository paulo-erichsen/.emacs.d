;; auto-complete
(require 'auto-complete-config)
(ac-config-default)
(add-to-list 'ac-dictionary-directories
             (expand-file-name "dict" malkav-vendor-dir)) ;; add custom dictionary folder
(add-to-list 'ac-modes 'opascal-mode) ;; enable AC automatically for specific modes

(provide 'malkav-auto-complete)
