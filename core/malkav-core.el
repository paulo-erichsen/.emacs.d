;;; Code:

;; open files from emacs in dired ~ select the line, then hit "C-c o" to open
;; http://www.emacswiki.org/emacs/OperatingOnFilesInDired
(require 'dired)
(defun malkav-dired-open-file ()
  "In dired, open the file named on this line."
  (interactive)
  (let* ((file (dired-get-filename nil t)))
    (call-process "xdg-open" nil 0 nil file)))
(define-key dired-mode-map (kbd "C-c o") 'malkav-dired-open-file)

(defun malkav-smart-open-line-above ()
    "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current mode."
    (interactive)
    (move-beginning-of-line nil)
    (newline-and-indent)
    (forward-line -1)
    (indent-according-to-mode))

(defun malkav-recompile-init ()
  "Byte-compile all your dotfiles again."
  (interactive)
  (byte-recompile-directory malkav-dir 0))

(defun malkav-wrap-with (s)
  "Create a wrapper function for smartparens using S."
  `(lambda (&optional arg)
     (interactive "P")
     (sp-wrap-with-pair ,s)))

(provide 'malkav-core)
;;; malkav-core.el ends here
