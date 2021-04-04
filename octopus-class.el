;;; octopus-class.el --- Class definition(s) and methods for octopus -*- lexical-binding: t -*-

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

;; This library provides classes and methods.

;;; Code:

(require 'eieio)
(require 'dash)
(require 'octopus-utils)
(require 'octopus-org-ql)

(declare-function project-find-file "project")

;;;; Types and classes

(defclass octopus-org-project-class ()
  ((project-dir :initarg :project-dir
                :type (or null string))
   (project-remote :initarg :project-remote
                   :type (or null string))
   (marker :initarg :marker
           :type marker)
   (timestamp-info :initarg :timestamp-info
                   :type (or null octopus-timestamp-info))))

(defclass octopus-org-project-group-class ()
  ((group-type :initarg :group-type
               :type symbol)
   (projects :initarg :projects
             :type list)
   (frecency-score
     :initarg :frecency-score
     :type number)))

;;;; Custom variables

(defcustom octopus-org-project-actions
  '((browse-dir
     :description "Browse the project directory"
     :slot project-dir
     :verify file-directory-p
     :dispatch octopus--browse-dir)
    (display-org-marker
     :description "Display the Org subtree"
     :slot marker
     :dispatch octopus--display-org-marker)
    (todo-list
     :description "Show a todo list"
     :slot project-dir
     :verify file-directory-p
     :dispatch octopus--project-todo-list)
    (find-file
     :description "Find a file in the project"
     :slot project-dir
     :verify file-directory-p
     :dispatch
     (lambda (dir)
       (let ((default-directory dir))
         (project-find-file)))))
  "Alist of actions."
  :group 'octopus
  :type '(alist :key-type symbol
                :value-type plist))

;;;; Search

(cl-defun octopus-org-project-groups (predicate
                                      &key
                                      (frecency t)
                                      (sort 'frecency))
  "Return a list of `octopus-org-project-group-class'.

PREDICATE is a QL query.

If FRECENCY is non-nil, scan all inactive timestamps in each
subtree and add the frecency score.

The result is sorted by SORT. By default, it is sorted by frecency."
  (--> (octopus--ql-select predicate
         :action `(octopus--org-project-from-subtree
                      :timestamps ,frecency))
    (octopus--group-org-projects 'dir it
                                 :frecency frecency)
    (octopus--sort-project-groups it sort)))

(defun octopus--sort-project-groups (groups key)
  "Sort GROUPS by KEY.

The groups must be a list of `octopus-org-project-group-class'.

KEY can be frecency or nil."
  (cl-ecase key
    (frecency (-sort (-on #'> (lambda (x)
                                (oref x frecency-score)))
                     groups))
    (nil groups)))

(cl-defun octopus--org-project-from-subtree (&key timestamps)
  "Construct an instance of `octopus-org-project-class'.

If TIMESTAMPS is non-nil, it scans timestamps."
  (declare (indent 1))
  (make-instance 'octopus-org-project-class
                 :project-dir (octopus--org-project-dir)
                 :project-remote (octopus--org-project-remote)
                 :marker (point-marker)
                 :timestamp-info (when timestamps
                                   (octopus--subtree-timestamp-info))))

(cl-defun octopus--group-org-projects (type projects &key frecency)
  "Group projects.

This returns a list of instances of
`octopus-org-project-group-class'. It groups projects by TYPE,
which can be either dir or remote. PROJECTS must be a list of
`octopus-org-project-class'.

Optionally, if FRECENCY is non-nil, groups are sorted by the
frecency score calculated from timestamps."
  (->> projects
       (-group-by (lambda (x)
                    (cl-ecase type
                      (dir (or (oref x project-dir)
                               (oref x project-remote)))
                      (remote (or (oref x project-remote)
                                  (oref x project-dir))))))
       (-map (pcase-lambda (`(,_ . ,projects))
               (make-instance 'octopus-org-project-group-class
                              :group-type type
                              :projects projects
                              :frecency-score
                              (when frecency
                                (octopus-timestamp-info-frecency
                                 (-reduce #'octopus-merge-timestamp-info
                                          (--map (slot-value it 'timestamp-info)
                                                 projects)))))))))

;;;; Invoking actions

(cl-defun octopus--run-action (action project &key dispatch)
  "Invoke an ACTION on PROJECT.

ACTION should be a symbol in `octopus-org-project-actions'.

DISPATCH can be a function that takes the data as an
argument. This is intended for testing. ."
  (let* ((plist (or (alist-get action octopus-org-project-actions)
                    (error "Undefined entry %s in octopus-org-project-actions" action)))
         (slot (plist-get plist :slot))
         ;; TODO: Allow using a custom reducer from (plist-get plist :reduce)
         (reducer (lambda (xs)
                    ;; If an empty list is given, the result will be nil,
                    ;; so you can handle fallback situations.
                    (pcase (-flatten-n 1 (-non-nil xs))
                      (`(,item) item)
                      ;; TODO: Add support for fallback
                      (`nil (cl-ecase slot
                              (project-dir
                               (error "No directory"))
                              (project-remote
                               (error "No remote"))))
                      (xs (cl-ecase slot
                            (project-dir
                             (octopus--pick-interactively "Project directory: " xs))
                            (project-remote
                             (error "TODO: Not implemented"))
                            (marker
                             (octopus--select-org-marker
                              "Select a subtree: " xs
                              :name "Org subtrees for the project")))))))
         (data (cl-etypecase project
                 (octopus-org-project-class
                  (slot-value project slot))
                 (octopus-org-project-group-class
                  (->> (oref project projects)
                       (-map (lambda (project) (slot-value project slot)))
                       (funcall reducer))))))
    (when-let (verify (plist-get plist :verify))
      (unless (funcall verify data)
        (error "Test failed on the value of %s: %s returned non-nil on %s"
               slot verify data)))
    (funcall (or dispatch (plist-get plist :dispatch)) data)))

(provide 'octopus-class)
;;; octopus-class.el ends here
