;;; octopus-org.el --- Org-related definitions and utilities for octopus -*- lexical-binding: t -*-

;; Copyright (C) 2021 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; URL: https://github.com/akirak/octopus.el

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library provides basic Org-related functionality used in the
;; package.

;;; Code:

(require 'org)

(require 'octopus-utils)

(defconst octopus-dir-property-name "PROJECT_DIR"
  "Org property that locates the root directory of a project.")

(defconst octopus-remote-repo-property-name "PROJECT_REMOTE_REPO"
  "Org property that locates the remote repository of a project.")

(defcustom octopus-org-files #'org-agenda-files
  "Function to retrieve a list of source Org files."
  :group 'octopus
  :type 'function)

(defcustom octopus-display-org-buffer-function
  #'pop-to-buffer
  ;; #'switch-to-buffer-other-window
  "Function used to display an Org buffer."
  :group 'octopus
  :type 'function)

(defcustom octopus-org-show-entry-hook
  '(org-narrow-to-subtree
    org-show-entry)
  "List of functions called after showing an Org entry.

These functions are called without arguments at an Org
heading by `octopus-project-org-root'."
  :group 'octopus
  :type 'hook)

(defcustom octopus-headline-format
  #'octopus-format-headline-1
  "Function used to format a headline for display.

This is used to build each candidate in `completing-read'-like
interfaces in the package.

The function is called with no arguments at the marker position."
  :group 'octopus
  :type 'function)

(defun octopus-format-headline-1 ()
  "Default implementation of `octopus-headline-format'."
  (format "%s: %s %s"
          (buffer-name)
          (org-format-outline-path
           (org-get-outline-path t t))
          (org-make-tag-string (org-get-tags))))

(defun octopus--display-org-marker (marker)
  "Display an Org MARKER using designated functions.

This uses `octopus-display-org-buffer-function' and
`octopus-org-tree-show-function'."
  (with-current-buffer (marker-buffer marker)
    (widen)
    (goto-char marker)
    (funcall octopus-display-org-buffer-function (current-buffer))
    (run-hooks 'octopus-org-show-entry-hook)))

(defsubst octopus-org-files ()
  "Return the Org files of interest.

This just calls `octopus-org-files'."
  (funcall octopus-org-files))

(defsubst octopus--org-project-dir ()
  "Return the root directory of a project set in the property, if any."
  (org-entry-get nil octopus-dir-property-name t))

(defsubst octopus--org-project-remote ()
  "Return the remote repository of a project set in the property, if any."
  (org-entry-get nil octopus-remote-repo-property-name t))

(defun octopus--org-up-project-root ()
  "If inside a project subtree, go to the root."
  (let ((initial (point))
        identity)
    (catch 'finish
      (while (not (setq identity (or (org-entry-get nil octopus-dir-property-name)
                                     (org-entry-get nil octopus-remote-repo-property-name))))
        (unless (org-up-heading-safe)
          (throw 'finish t))))
    (if identity
        (message "Go to the subtree root for project %s" identity)
      (goto-char initial)
      (message "Not inside a project subtree"))))

(provide 'octopus-org)
;;; octopus-org.el ends here
