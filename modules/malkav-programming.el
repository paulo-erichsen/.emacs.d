;;; malkav-programming.el --- Emacs Malkav: prog-mode configuration
;;
;; Copyright © 2011-2016 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/malkav
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Some basic prog-mode configuration and programming related utilities.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(defun malkav-local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t))

(defun malkav-font-lock-comment-annotations ()
  "Highlight a bunch of well known comment annotations.

This functions should be added to the hooks of major modes for programming."
  (font-lock-add-keywords
   nil '(("\\<\\(\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\):\\)"
          1 font-lock-warning-face t))))

;; show the name of the current function definition in the modeline
(require 'which-func)
(which-function-mode 1)

;; in Emacs 24 programming major modes generally derive from a common
;; mode named prog-mode; for others, we'll arrange for our mode
;; defaults function to run malkav-prog-mode-hook directly.  To
;; augment and/or counteract these defaults your own function
;; to malkav-prog-mode-hook, using:
;;
;;     (add-hook 'malkav-prog-mode-hook 'my-prog-mode-defaults t)
;;
;; (the final optional t sets the *append* argument)

;; smart curly braces
;; (require 'smartparens)
;; (sp-pair "{" nil :post-handlers
;;          '(((lambda (&rest _ignored)
;;               (malkav-smart-open-line-above)) "RET")))

;; enlist a more liberal guru
(setq guru-warn-only t)

(defun malkav-prog-mode-defaults ()
  "Default coding hook, useful with any programming language."
  (when (and (executable-find ispell-program-name)
             malkav-flyspell)
    (flyspell-prog-mode))
  (when malkav-guru
    (guru-mode +1))
  ;; (smartparens-mode +1)
  (malkav-enable-whitespace)
  (malkav-local-comment-auto-fill)
  ;; (malkav-font-lock-comment-annotations))
  )

(setq malkav-prog-mode-hook 'malkav-prog-mode-defaults)

(add-hook 'prog-mode-hook (lambda ()
                            (run-hooks 'malkav-prog-mode-hook)))

;; enable on-the-fly syntax checking
(if (fboundp 'global-flycheck-mode)
    (global-flycheck-mode +1)
    (add-hook 'prog-mode-hook 'flycheck-mode))

(provide 'malkav-programming)
;;; malkav-programming.el ends here
