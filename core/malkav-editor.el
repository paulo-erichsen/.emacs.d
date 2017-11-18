;;; malkav-editor.el -- Editor settings

;;; Commentary:

;; sets editor settings

;;; Code:

;; delete white-spaces on save
(add-hook 'before-save-hook 'whitespace-cleanup)

;; flyspell-mode does spell-checking on the fly as you type
(require 'flyspell)
(setq ispell-program-name "aspell" ; use aspell instead of ispell
      ispell-extra-args '("--sug-mode=ultra"))

(defun malkav-enable-whitespace ()
  "Enable `whitespace-mode' if `malkav-whitespace' is not nil."
  (when malkav-whitespace
    ;; keep the whitespace decent all the time (in this buffer)
    (add-hook 'before-save-hook 'malkav-cleanup-maybe nil t)
    (whitespace-mode +1)))

(defun malkav-cleanup-maybe ()
  "Invoke `whitespace-cleanup' if `malkav-clean-whitespace-on-save' is not nil."
  (when malkav-clean-whitespace-on-save
    (whitespace-cleanup)))

;; (add-hook 'text-mode-hook 'malkav-enable-flyspell)
;; (add-hook 'text-mode-hook 'malkav-enable-whitespace)

;; store all backup files and autosave files in the tmp dir ;; `echo $TMPDIR`
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(provide 'malkav-editor)
