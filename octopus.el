;;; octopus.el --- Integration between Org and project.el -*- lexical-binding: t -*-

;; Copyright (C) 2021 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (project "0.5.3") (org-ql "0.6") (dash "2.18"))
;; Keywords: outlines convenience
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

;; FIXME

;;; Code:

(require 'octopus-org-ql)
(require 'octopus-utils)

(defgroup octopus nil
  "FIXME"
  :group 'org
  :group 'project)

;;;; Custom variables

(defcustom octopus-git-repository-list-function
  #'magit-list-repos
  "Function that returns a list of local Git repositories.

This can be any function that takes no argument and returns a
list of repository directories for your projects.

An example is `magit-list-repos'."
  :type 'function)

(defsubst octopus--repo-list ()
  "Return a list of repositories using `octopus-git-repository-list-function'."
  (funcall octopus-git-repository-list-function))

(defcustom octopus-browse-project-dir-function
  #'magit-status
  "Function used to open a repository.

This should be a function that takes a repository directory and
displays its status of contents.

You can use `magit-status', `dired', etc."
  :type 'function)

(defsubst octopus--browse-project-dir (dir)
  "Call `octopus-browse-project-dir-function' on DIR."
  (funcall octopus-browse-project-dir-function dir))

;;;; Macros

(defmacro octopus--org-put-new-property-with (property exp)
  "If PROPERTY is unset on the Org entry, evalate EXP to fetch a value."
  (declare (indent 1))
  `(progn
     (assert (and (derived-mode-p 'org-mode)
                  (not (org-before-first-heading-p))))
     (when (org-entry-get nil ,property t)
       (user-error "This entry already has %s property set" ,property))
     (org-entry-put nil ,property ,exp)))

(defmacro octopus-select-from-other-windows (prompt exp)
  "Get a value from the buffers in other windows.

This displays PROMPT and asks the user to pick one of the values of EXP
in other windows' buffers."
  (declare (indent 1))
  `(let* ((current ,exp)
          (options (->> (let (results)
                          (walk-windows
                           (lambda (w)
                             (push (with-current-buffer (window-buffer w)
                                     ,exp)
                                   results)))
                          results)
                        (-non-nil)
                        (-uniq)
                        (-remove-item current))))
     (completing-read ,prompt options)))

(defmacro octopus-select-from-session (prompt exp)
  "Get a value from the current session.

It prints PROMPT and asks the user to pick a value of EXP.

For now, `octopus-select-from-other-windows' is used."
  (declare (indent 1))
  `(octopus-select-from-other-windows ,prompt ,exp))

;;;; Find an Org tree for the project

;;;###autoload
(defun octopus-display-project-org-subtree (&optional arg)
  "Display an Org subtree associated with the current project.

This function lets the user select an Org subtree associated with
the root directory or the remote repository of the current
project.

If there are multiple subtrees, the user is allowed to choose one.

If ARG is non-nil, the user can choose a subtree of any project,
not limited to the current one.

You can tweak the behavior by customizing
`octopus-display-org-buffer-function' and
`octopus-org-tree-show-function'."
  (interactive "P")
  (let ((marker (--> (if arg
                         (octopus--ql-select '(default-and (any-project)))
                       (octopus--ql-select `(project ,(octopus--project-root))
                         :sort #'octopus--dir-element-first))
                  (octopus--single-or it
                                      (octopus--user-select-org-marker
                                       "Select a subtree to display: "
                                       it "Org subtrees for the project")
                                      "No subtree for the project"))))
    (octopus--display-org-marker marker)))

;;;; Find a local repository for the Org tree

;;;###autoload
(defun octopus-browse-project-root ()
  "Display the root directory of a project associated with the Org context.

This function looks for an Org property about a project from the
current Org entry and its ancestors and visit its root directory.
It uses `octopus-browse-project-dir-function' to display a directory."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Must be run in org-mode"))
  (assert (not (org-before-first-heading-p)))
  (if-let (root (octopus--org-project-dir))
      (octopus--browse-project-dir root)
    (if-let (remote-repo (octopus--org-project-remote))
        (pcase (->> (octopus--repo-list)
                    (-filter (lambda (root)
                               (string-equal remote-repo
                                             (octopus--abbreviate-remote-url root)))))
          (`(,x)
           (octopus--browse-project-dir x))
          (`nil
           (user-error "No local copy of the repository %s" remote-repo))
          (xs
           (octopus--browse-project-dir (completing-read "Local copy: " xs))))
      (user-error "No property for the project identity"))))

;;;; Set a project property

;;;###autoload
(defun octopus-org-set-project-dir ()
  "Set the project directory property in Org."
  (interactive)
  (octopus--org-put-new-property-with octopus-dir-property-name
                                      (octopus-select-from-session "Project directory: "
                                        (octopus--project-root))))

;;;###autoload
(defun octopus-org-set-project-remote-repo (&optional arg)
  "Set the project remote repository property in Org.

If ARG is non-nil, it lets the user select a remote repository
URL instead."
  (interactive "P")
  (octopus--org-put-new-property-with octopus-remote-repo-property-name
                                      (if arg
                                          (->> (octopus--repo-list)
                                               (-map #'octopus--abbreviate-remote-url)
                                               (-non-nil)
                                               (-uniq)
                                               (completing-read "Origin: "))
                                        (octopus-select-from-session "Origin: "
                                                                     (octopus--abbreviate-remote-url)))))

;;;; Project todo list

(defcustom octopus-project-todo-super-groups
  '((:todo "DONE" :order 5)
    (:auto-planning t)
    (:todo t))
  "`org-super-agenda' groups used in `octopus-project-todo-list' command."
  :type 'sexp)

(defun octopus-project-todo-list (arg)
  "Display a todo list of the current project.

Alternatively, you can use this command to display a todo list of
a project based on the project root (with a single universal
argument ARG) or based on the remote repository (with two universal
arguments).

Items a grouped by `octopus-project-todo-super-groups'."
  (interactive "P")
  (pcase arg
    ('(16)
     (octopus--project-todo-list-on-remote
      (completing-read
       (format "Display a todo list for a project: "
               octopus-remote-repo-property-name)
       (octopus--collect-org-property-values
        octopus-remote-repo-property-name))))
    ('(4)
     (octopus--project-todo-list
      (completing-read
       (format "Display a todo list for a project: "
               octopus-dir-property-name)
       (octopus--collect-org-property-values
        octopus-dir-property-name))))
    (_
     (octopus--project-todo-list
      (octopus--project-root)))))

(defun octopus--project-todo-list (root)
  "Display a todo list of a project at ROOT."
  (let ((default-directory root))
    (octopus--ql-search
        `(default-and
           (todo)
           (not (todo "DONE"))
           (ancestors (project ,root)))
      :title (format "Project %s" root)
      :super-groups octopus-project-todo-super-groups)))

(defun octopus--project-todo-list-on-remote (remote)
  "Display a todo list of a project on REMOTE."
  (octopus--ql-search
      `(default-and (todo)
         (not (todo "DONE"))
         (ancestors (project-remote-property ,remote)))
    :title (format "Project %s" remote)
    :super-groups octopus-project-todo-super-groups))

(provide 'octopus)
;;; octopus.el ends here
