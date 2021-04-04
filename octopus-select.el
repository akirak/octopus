;;; octopus-select.el --- Select interface -*- lexical-binding: t -*-

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

;; This library provides utilities for selection interfaces in the package.

;;; Code:

(require 'dash)
(require 'eieio)
(require 'ts)
(require 'org)
(require 'octopus-class)
(require 'octopus-utils)

(defgroup octopus-face
  nil
  "Faces for Octopus."
  :group 'octopus)

(defcustom octopus-select-project-extra-lines-format
  #'octopus-select-format-extra-lines-1
  "Function for formatting extra lines of project entries.

This should be a function that takes a list of Org markers as an
argument and returns a string or nil."
  :type 'function)

;;;; Faces
(defface octopus-select-remote-face
  '((t (:inherit font-lock-constant-face)))
  "Face for remote repository URLs."
  :group 'octopus-face)

(defface octopus-remote-delimiter-face
  '((t (:inherit font-lock-type-face)))
  "Face for delimiters."
  :group 'octopus-face)

(defface octopus-remote-directory-face
  '((t (:inherit font-lock-string-face)))
  "Face for existing directory paths."
  :group 'octopus-face)

(defface octopus-remote-nonexistent-directory-face
  '((t (:inherit font-lock-comment-face)))
  "Face for non-existent directory paths."
  :group 'octopus-face)

(defface octopus-remote-time-face
  '((t (:inherit font-lock-doc-face)))
  "Face for time strings."
  :group 'octopus-face)

(defface octopus-remote-tag-face
  '((t (:inherit org-tag)))
  "Face for Org tags."
  :group 'octopus-face)

;;;; Formatting functions

;;;;; Generics

(cl-defgeneric octopus-format-candidate (x)
  "Format a candiate X for a completion interface.")

(cl-defgeneric octopus-format-candidate-multiline (x)
  "Format a candiate X for a completion interface that supports multi-line entries."
  (octopus-format-candidate x))

(cl-defmethod octopus-format-candidate-multiline ((x octopus-org-project-group-class))
  "Format a candiate X for a completion interface that supports multi-line entries."
  (let* ((projects (oref x projects))
         (project (car projects))
         (remote (oref project project-remote))
         (dir (oref project project-dir))
         (time (->> projects
                    (-map (lambda (p)
                            (when-let (info (oref p timestamp-info))
                              (ts-unix (octopus-timestamp-info-last-ts info)))))
                    (-non-nil)
                    (-max)))
         (markers (-map (lambda (p) (oref p marker)) projects))
         (extras (when octopus-select-project-extra-lines-format
                   (funcall octopus-select-project-extra-lines-format markers))))
    (octopus-select-format-project :remote remote
                                   :dir dir
                                   :time time
                                   :extras extras)))

;;;;; Formatting candidates

(cl-defun octopus-select-format-project (&key remote dir time extras)
  "Format a project given data.

REMOTE, DIR, TIME, and EXTRAS are optional strings describing the
project or project group."
  (->> (list (when remote
               (propertize remote 'face 'octopus-select-remote-face))
             (when (and remote dir)
               (propertize " | " 'face 'octopus-remote-delimiter-face))
             (when dir
               (propertize dir 'face (if (file-directory-p dir)
                                         'octopus-remote-directory-face
                                       'octopus-remote-nonexistent-directory-face)))
             "  "
             (when time
               (propertize (octopus--format-time time)
                           'face 'octopus-remote-time-face))
             (when extras
               "\n")
             extras)
       (-non-nil)
       (string-join)))

;;;;; Formatting particular fields

(defcustom octopus-select-hidden-tags
  '("ARCHIVE")
  "List of tags to hide from the selection interface."
  :type '(repeat string))

(defcustom octopus-select-project-extra-fields
  '((language (org-entry-get nil "LANGUAGE" t)
              (lambda (languages)
                (string-join (-uniq languages) " ")))
    (tags (org-get-tags)
          (lambda (tags)
            (-some-> (-uniq (-non-nil tags))
              (-difference octopus-select-hidden-tags)
              (string-join " ")
              (propertize 'face 'org-tag)))))
  "Alist of extra fields in `octopus-select-format-extra-lines-1'."
  :type '(repeat (list symbol
                       sexp
                       function)))

(defun octopus-select-format-extra-lines-1 (markers)
  "Format extra lines of projects at MARKERS."
  (let ((fields (->> markers
                     (--map (org-with-point-at it
                              (-map (pcase-lambda (`(,key ,exp . ,_))
                                      (cons key
                                            (eval exp)))
                                    octopus-select-project-extra-fields)))
                     (-flatten-n 1)
                     (-group-by #'car)
                     (-map (pcase-lambda (`(,key . ,entries))
                             (when-let (xs (-flatten-n 1 (-map #'cdr entries)))
                               (funcall (nth 2 (assoc key octopus-select-project-extra-fields))
                                        xs))))
                     (-non-nil))))
    (when fields
      (concat "  "
              (string-join fields (propertize " | " 'face 'octopus-remote-delimiter-face))))))

(provide 'octopus-select)
;;; octopus-select.el ends here
