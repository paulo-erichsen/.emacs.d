;; open files from emacs in dired ~ select the line, then hit "C-c o" to open
;; http://www.emacswiki.org/emacs/OperatingOnFilesInDired
(require 'dired)
(defun dired-open-file ()
  "In dired, open the file named on this line."
  (interactive)
  (let* ((file (dired-get-filename nil t)))
    (call-process "xdg-open" nil 0 nil file)))
(define-key dired-mode-map (kbd "C-c o") 'dired-open-file)
