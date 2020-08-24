;;; .emacs.d/init.el -- the `user-init-file'
;; this is a modified version of Batsov's init file for my needs
;;
;;; init.el --- Malkav's configuration entry point.
;;
;; Copyright (c) 2011-2016 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: http://batsov.com/prelude
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file simply sets up the default load path and requires
;; the various modules defined within Emacs Prelude.

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

;; Always load newest byte code

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(when (< emacs-major-version 27)
  (require 'package-initialize))

(setq load-prefer-newer t)

;; set some variables
(defvar malkav-dir (file-name-directory load-file-name)
  "The root dir of the Emacs Malkav distribution.")
(defvar malkav-core-dir (expand-file-name "core" malkav-dir)
  "The home of Malkav's core functionality.")
(defvar malkav-modules-dir (expand-file-name  "modules" malkav-dir)
  "This directory houses all of the built-in Malkav modules.")
(defvar malkav-vendor-dir (expand-file-name "vendor" malkav-dir)
  "This directory houses packages that are not yet available in ELPA (or MELPA).")
(defvar malkav-savefile-dir (expand-file-name "savefile" malkav-dir)
  "This folder stores all the automatically generated save/history-files.")
(defvar malkav-modules-file (expand-file-name "malkav-modules.el" malkav-dir)
  "This files contains a list of modules that will be loaded by Malkav.")

(defun malkav-add-subfolders-to-load-path (parent-dir)
 "Add all level PARENT-DIR subdirs to the `load-path'."
 (dolist (f (directory-files parent-dir))
   (let ((name (expand-file-name f parent-dir)))
     (when (and (file-directory-p name)
                (not (string-prefix-p "." f)))
       (add-to-list 'load-path name)
       (malkav-add-subfolders-to-load-path name)))))

;; add Malkav's directories to Emacs's `load-path'
(add-to-list 'load-path malkav-core-dir)
(add-to-list 'load-path malkav-modules-dir)
(add-to-list 'load-path malkav-vendor-dir)
(malkav-add-subfolders-to-load-path malkav-vendor-dir)

;; the core stuff
(require 'malkav-packages)
(require 'malkav-custom)
(require 'malkav-ui)
(require 'malkav-core)
;; (require 'malkav-mode)
(require 'malkav-editor)
(require 'malkav-global-keybindings)

;; the modules
(if (file-exists-p malkav-modules-file)
    (load malkav-modules-file)
  (message "Missing modules file %s" malkav-modules-file)
  (message "You can get started by copying the bundled example file"))

;; Linux specific settings
(when (eq system-type 'gnu/linux)
  (require 'malkav-linux))

;; config changes made through the customize UI will be store here
(setq custom-file (expand-file-name "custom.el" malkav-dir))
(load-file custom-file)

;;; init.el ends here
