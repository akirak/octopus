;;; octopus-org-ql.el --- Org-ql for octopus -*- lexical-binding: t -*-

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

;; This library provides infrastructure for the package with help of
;; org-ql package.

;;; Code:

(require 'octopus-utils)
(require 'octopus-org)

(require 'org-ql)

(declare helm-octopus-org-marker "ext:helm-octopus")

;;;; Custom variables

(defcustom octopus-org-ql-default-predicate
  '(not (tags "ARCHIVE"))
  "Default `org-ql' predicate for various commands."
  :type 'sexp)

;;;; Completing-read like interface for Org markers

(defcustom octopus-org-marker-select-interface
  (if (require 'helm nil t)
      'helm
    #'completing-read)
  "Completion interface used to select Org markers.

If available, `helm' is recommended.

Alternatively, you can specify a function which is compatible
with `completing-read'."
  :type '(choice (const :tag "Helm" helm)
                 function))

(cl-defun octopus--user-select-org-marker (prompt markers &key name)
  "Make the user select an Org marker from candidates.

When `octopus-org-marker-select-interface' is set to helm,
PROMPT, MARKERS, and NAME are passed to
`helm-octopus-org-marker', which see.

Otherwise, the function specified in the variable are used as a
completion interface."
  (pcase octopus-org-marker-select-interface
    (`helm
     (helm-octopus-org-marker prompt markers
                              ((predicate functionp)
                               :name name)))
    (_
     (->> (funcall octopus-org-marker-select-interface
                   prompt
                   (-map (lambda (marker)
                           (-> (org-with-point-at marker
                                 (funcall octopus-headline-format))
                               (propertize 'org-marker marker)))
                         markers))
          (get-char-property 0 'org-marker choice)))))

(defmacro octopus--single-or (items exp &optional null-message)
  "If necessary, pick an item from multiple candidates using a given expression.

If ITEMS contain only one element, return it.

If there are multiple items, evalate EXP to pick an item. The
expression should be a call to `completing-read'-like interface,
e.g. `octopus--user-select-org-marker'.

If the first argument is nil, it throws an error. You can specify
the error message as NULL-MESSAGE."
  (declare (indent 1))
  `(pcase ,items
     (`nil
      (error ,(or null-message "Empty list")))
     (`(,item)
      item)
     (_
      ,exp)))

;;;; Building queries

(defun octopus--ql-expand (exp)
  "Expand project-related ql predicates in EXP."
  (declare (indent 0))
  (macroexpand-all
   exp
   '((default-and
       . (lambda (&rest args)
           (if octopus-org-ql-default-predicate
               `(and ,octopus-org-ql-default-predicate
                     ,@args)
             `(and ,@args))))
     (project-dir-property
      . (lambda (&rest args)
          `(property ,octopus-dir-property-name ,@args)))
     (project-remote-property
      . (lambda (&rest args)
          `(property ,octopus-remote-repo-property-name ,@args)))
     (any-project
      . (lambda ()
          '(or (project-dir-property)
               (project-remote-property))))
     (project
      . (lambda (root)
          `(and (project-dir-property ,(abbreviate-file-name
                                        (file-name-as-directory root)))
                ,@(when-let (remote (octopus--abbreviate-remote-url root))
                    (list `(project-remote-property ,remote)))))))))

;;;; Querying
;; Call `org-ql-select' to get a list of markers matching a certain condition.
(cl-defun octopus--ql-select (predicate &key (action #'point-marker) sort)
  "An internal wrapper for `org-ql-select'.

PREDICATE, ACTION, and SORT are passed to the function, which see."
  (declare (indent 1))
  (org-ql-select (octopus-org-files)
    (octopus--ql-expand predicate)
    :action action
    :sort sort))

(cl-defun octopus--ql-search (predicate &key sort title super-groups)
  "An internal wrapper for `org-ql-search'.

PREDICATE, SORT, TITLE, and SUPER-GROUPS are passed to
`org-ql-search', which see."
  (declare (indent 1))
  (org-ql-search (octopus-org-files)
    (octopus--ql-expand predicate)
    :super-groups super-groups
    :title title
    :sort sort))

;;;; Compare functions for sorting

(defconst octopus-dir-property-symbol
  (intern (concat ":" octopus-dir-property-name)))
(defconst octopus-remote-repo-property-symbol
  (intern (concat ":" octopus-remote-repo-property-name)))

(defun octopus--dir-element-first (a b)
  "Compare two Org elements so entries with the directory property come first.

A and B must be Org elements."
  (funcall (-on #'>
                (lambda (x)
                  (cond
                   ((org-element-property octopus-dir-property-symbol x)
                    2)
                   ((org-element-property octopus-remote-repo-property-symbol x)
                    1))))
           a b))

(defun octopus--collect-org-property-values (property)
  "Return all values of PROPERTY in `octopus-org-files'."
  (->> (octopus-org-files)
       (-map (lambda (file)
               (with-current-buffer (or (find-buffer-visiting file)
                                        (find-file-noselect file))
                 (org-property-values property))))
       (-non-nil)
       (-flatten-n 1)
       (-uniq)
       ;; I don't know if lexicographical sorting suits every situation.
       ;; Maybe using frecency will be better.
       (-sort #'string<)))

(provide 'octopus-org-ql)
;;; octopus-org-ql.el ends here
