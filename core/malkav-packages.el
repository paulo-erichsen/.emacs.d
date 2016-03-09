;;; malkav-packages.el --- Emacs Malkav: default package selection.
;;
;; This file was written by Batsov, but I've modified it for my needs :)
;;
;; Copyright © 2011-2016 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Takes care of the automatic installation of all the packages required by
;; Emacs Prelude.

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

(require 'cl)
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

;; set package-user-dir to be relative to Malkav install path
(setq package-user-dir (expand-file-name "elpa" malkav-dir))
(package-initialize)  ;; this loads auto-complete into the path from MELPA
(setq package-enable-at-startup nil) ;; don't initialize the package again `after-init-hook`

(defvar malkav-packages
  '(
    auto-complete
    lua-mode
    markdown-mode
    rubocop
    ;; ace-window
    ;; avy
    ;; anzu
    ;; beacon
    ;; browse-kill-ring
    ;; dash
    ;; discover-my-major
    ;; diff-hl
    ;; diminish
    ;; easy-kill
    ;; epl
    ;; expand-region
    ;; flycheck
    ;; gist
    ;; git-timemachine
    gitconfig-mode
    gitignore-mode
    ;; god-mode
    ;; grizzl
    guru-mode
    ;; imenu-anywhere
    ;; ov
    ;; projectile
    magit
    ;; move-text
    ;; operate-on-number
    ;; smart-mode-line
    ;; smartparens
    ;; smartrep
    ;; undo-tree
    ;; volatile-highlights
    ;; zenburn-theme
    gist
    ;; zop-to-char
    )
  "A list of packages to ensure are installed at launch.")

(defun malkav-packages-installed-p ()
  "Check if all packages in `malkav-packages' are installed."
  (every #'package-installed-p malkav-packages))

(defun malkav-require-package (package)
  "Install PACKAGE unless already installed."
  (unless (memq package malkav-packages)
    (add-to-list 'malkav-packages package))
  (unless (package-installed-p package)
    (package-install package)))

(defun malkav-require-packages (packages)
  "Ensure PACKAGES are installed.
Missing packages are installed automatically."
  (mapc #'malkav-require-package packages))

(define-obsolete-function-alias 'malkav-ensure-module-deps 'malkav-require-packages)

(defun malkav-install-packages ()
  "Install all packages listed in `malkav-packages'."
  (unless (malkav-packages-installed-p)
    ;; check for new packages (package versions)
    (message "%s" "Emacs Malkav is now refreshing its package database...")
    (package-refresh-contents)
    (message "%s" " done.")
    ;; install the missing packages
    (malkav-require-packages malkav-packages)))

;; run package installation
(malkav-install-packages)

(provide 'malkav-packages)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; malkav-packages.el ends here
