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
(require 'org-element)
(require 'cl-lib)
(require 'ts)

(require 'octopus-utils)

(defconst octopus-dir-property-name "OCTOPUS_DIR"
  "Org property that locates the root directory of a project.")

(defconst octopus-remote-repo-property-name "OCTOPUS_REMOTE_REPO"
  "Org property that locates the remote repository of a project.")

(defconst octopus-clone-destination-property-name "OCTOPUS_CLONE_DESTINATION"
  "Org property that specifies the destination of git clone command.")

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
  '(org-show-entry
    org-narrow-to-subtree)
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

(defun octopus-format-headline-1 (&optional width)
  "Default implementation of `octopus-headline-format'.

WIDTH is an optional width of the output string in characters."
  (format "%s: %s %s"
          (buffer-name)
          (org-format-outline-path
           (org-get-outline-path t t)
           ;; If there are many entries, this can slow down the
           ;; performance, but it probably won't matter, assuming it
           ;; is used to select an entry from 10 or less root
           ;; subtrees.
           (or width (frame-width)))
          (org-make-tag-string (org-get-tags))))

(defun octopus--display-org-marker (marker)
  "Display an Org MARKER using designated functions.

This uses `octopus-display-org-buffer-function' and
`octopus-org-tree-show-function'."
  (with-current-buffer (marker-buffer marker)
    (widen)
    (goto-char marker)
    (run-hooks 'octopus-org-show-entry-hook)
    (funcall octopus-display-org-buffer-function (current-buffer))))

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

(defun octopus--subtree-timestamp-info ()
  "Return statistic information on timestamps in the subtree."
  (org-with-wide-buffer
   ;; If the entry is closed, use its closed time as the latest
   ;; activity in the entire subtree.
   (or (octopus--closed-subtree-timestamp-info)
       (octopus--collect-timestamp-info
        (save-excursion
          (org-end-of-subtree))))))

(defun octopus--closed-subtree-timestamp-info ()
  "If the entry is closed and old enough, return its `octopus-timestamp-info'."
  (when-let (closed (and (org-entry-is-done-p)
                         (org-entry-get nil "CLOSED")))
    (let* ((last-ts-unix (float-time
                          (org-timestamp-to-time
                           (org-timestamp-from-string
                            closed))))
           (time-diff (- (float-time) last-ts-unix)))
      ;; The frecency score will be 0 if the timestamp is more
      ;; than 7776000 seconds ago, so you don't have to count
      ;; timestamps in that case.
      (when (> time-diff 7776000)
        (make-octopus-timestamp-info :last-ts (make-ts :unix last-ts-unix)
                                     :count 0)))))

(defun octopus--collect-timestamp-info (end)
  "Collect statistic information on timestamps till END."
  (if (> (point) end)
      (make-octopus-timestamp-info :last-ts nil
                                   :count 0)
    (let ((re (org-re-timestamp 'inactive))
          last-ts
          (ts-count 0))
      (while (re-search-forward re end t)
        (let ((ts (make-ts
                   :unix (float-time
                          (org-timestamp-to-time
                           (org-timestamp-from-string (match-string 0)))))))
          (when (or (not last-ts)
                    (ts> ts last-ts))
            (setq last-ts ts))
          (cl-incf ts-count)))
      (make-octopus-timestamp-info :last-ts last-ts
                                   :count ts-count))))

(defun octopus--entry-timestamp-info ()
  "Return statistic information on timestamps in the entry."
  (org-with-wide-buffer
   (or (octopus--closed-subtree-timestamp-info)
       (octopus--collect-timestamp-info
        (org-entry-end-position)))))

(provide 'octopus-org)
;;; octopus-org.el ends here
