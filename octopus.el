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

;; octopus.el provides an integration between Org and project.el.

;;; Code:

(require 'octopus-org-ql)
(require 'octopus-utils)

(defgroup octopus nil
  "Integration between Org and project.el."
  :group 'org
  :group 'project)

;;;; Custom variables

(defcustom octopus-repo-list-fn
  #'magit-list-repos
  "Function that returns a list of local Git repositories.

This can be any function that takes no argument and returns a
list of repository directories for your projects.

An example is `magit-list-repos'."
  :type 'function)

(defsubst octopus--repo-list ()
  "Return a list of repositories using `octopus-repo-list-fn'."
  (funcall octopus-repo-list-fn))

(defcustom octopus-browse-dir-fn
  #'magit-status
  "Function used to open a repository.

This should be a function that takes a repository directory and
displays its status of contents.

You can use `magit-status', `dired', etc."
  :type 'function)

(defsubst octopus--browse-dir (dir)
  "Call `octopus-browse-dir-fn' on a project DIR."
  (funcall octopus-browse-dir-fn dir))

(defcustom octopus-org-dwim-commands t
  "FIXME"
  :type 'boolean)

(defcustom octopus-session-value-source 'buffers
  "Source of session values.

Some commands such as `octopus-org-set-project-dir' and
`octopus-org-set-project-remote-repo' lets the user pick a value
from the session.

This option lets the user select a buffer from it."
  :type '(choice (const windows)
                 (const buffers)))

;;;; Macros

(defmacro octopus--org-put-property-from-exp-once (property exp)
  "If PROPERTY is unset on the Org entry, evalate EXP to fetch a value."
  (declare (indent 1))
  `(progn
     (assert (and (derived-mode-p 'org-mode)
                  (not (org-before-first-heading-p))))
     (when (org-entry-get nil ,property t)
       (user-error "This entry already has %s property set" ,property))
     (org-entry-put nil ,property ,exp)))

(defsubst octopus--uniq-files (files)
  "Remove duplicates from FILES."
  (cl-remove-duplicates files :test #'file-equal-p))

(defmacro octopus--session-values (exp)
  "Evaluate EXP in each directory in the session and remove duplicates."
  `(->> (cl-ecase octopus-session-value-source
          (windows (walk-windows #'window-buffer))
          (buffers (buffer-list)))
        (--map (buffer-local-value 'default-directory it))
        (-non-nil)
        (-map #'expand-file-name)
        (octopus--uniq-files)
        (--map (let ((default-directory it))
                 ,exp))
        (-non-nil)
        (-uniq)))

;;;; Find an Org tree for the project

;;;###autoload
(defun octopus-project-org-root (&optional all interactive)
  "Display an Org subtree associated with the current project.

This function lets the user select an Org subtree associated with
the root directory or the remote repository of the current
project.

If there are multiple subtrees, the user is allowed to choose one.

If ALL is non-nil or C-u is given, the user can choose a subtree
of any project, not limited to the current one.

If INTERACTIVE, it displays the subtree using
`octopus-display-org-buffer-function' and
`octopus-org-tree-show-function'.

Otherwise, it returns a marker."
  (interactive "P")
  (if (and (not all)
           octopus-org-dwim-commands
           (derived-mode-p 'org-mode))
      (octopus--org-up-project-root)
    (let ((marker (--> (if all
                           (octopus--ql-select '(default-and (any-project)))
                         (octopus--ql-select `(project ,(octopus--project-root))
                           :sort #'octopus--dir-element-first))
                    (octopus--single-or it
                      (octopus--select-org-marker
                       "Select a subtree to display: " it
                       :name "Org subtrees for the project")
                      "No subtree for the project"))))
      (if (or interactive (called-interactively-p))
          (octopus--display-org-marker marker)
        marker))))

;;;; Find a local repository for the Org tree

;;;###autoload
(defun octopus-project-dir (&optional interactive)
  "Display the root directory of a project associated with the Org context.

This function looks for an Org property about a project from the
current Org entry and its ancestors and visit its root directory.
It uses `octopus-browse-dir-fn' to display a directory."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Must be run in org-mode"))
  (assert (not (org-before-first-heading-p)))
  (if-let (root (octopus--org-project-root))
      (if (or interactive (called-interactively-p))
          (octopus--browse-dir root)
        root)
    (user-error "No property for the project identity")))

(defun octopus--org-project-root ()
  "Get the root directory of the Org context, possibly with a remote."
  (or (octopus--org-project-dir)
      (-some->> (octopus--org-project-remote)
        (octopus--find-repository-by-remote-url))))

(defun octopus--find-repository-by-remote-url (remote-repo)
  "Find a local repository matching REMOTE-REPO."
  (--> (-filter (lambda (root)
                  (string-equal remote-repo
                                (octopus--abbreviate-remote-url root)))
                (octopus--repo-list))
    (octopus--single-or it
      (completing-read "Local copy: " it)
      (format "No local copy of the repository %s" remote-repo))))

;;;; Set a project property

;;;###autoload
(defun octopus-org-set-project-dir ()
  "Set the project directory property in Org."
  (interactive)
  (octopus--org-put-property-from-exp-once octopus-dir-property-name
    (completing-read "Project directory: "
                     (octopus--uniq-files
                      (octopus--session-values (octopus--project-root))))))

;;;###autoload
(defun octopus-org-set-project-remote-repo (&optional arg)
  "Set the project remote repository property in Org.

If ARG is non-nil, it lets the user select a remote repository
URL instead."
  (interactive "P")
  (octopus--org-put-property-from-exp-once octopus-remote-repo-property-name
    (if arg
        (->> (octopus--repo-list)
             (-map #'octopus--abbreviate-remote-url)
             (-non-nil)
             (-uniq)
             (completing-read "Origin: "))
      (completing-read "Origin: "
                       (octopus--session-values
                        (octopus--abbreviate-remote-url default-directory))))))

;;;; Project todo list

(defcustom octopus-todo-super-groups
  '((:todo "DONE" :order 5)
    (:auto-planning t)
    (:todo t))
  "`org-super-agenda' groups used in `octopus-todo-list' command."
  :type 'sexp)

;;;###autoload
(defun octopus-todo-list (arg)
  "Display a todo list of the current project.

Alternatively, you can use this command to display a todo list of
a project based on the project root (with a single universal
argument ARG) or based on the remote repository (with two universal
arguments).

Items a grouped by `octopus-todo-super-groups'."
  (interactive "P")
  (pcase arg
    ('(16)
     (octopus--remote-todo-list
      (completing-read
       (format "Display a todo list for a project: "
               octopus-remote-repo-property-name)
       (octopus--org-property-values
        octopus-remote-repo-property-name))))
    ('(4)
     (octopus--project-todo-list
      (completing-read
       (format "Display a todo list for a project: "
               octopus-dir-property-name)
       (octopus--org-property-values
        octopus-dir-property-name))))
    (_
     (octopus--project-todo-list
      (if (and octopus-org-dwim-commands
               (derived-mode-p 'org-mode))
          (octopus--org-project-root)
        (octopus--project-root))))))

(defun octopus--project-todo-list (root)
  "Display a todo list of a project at ROOT."
  (let ((default-directory root))
    (octopus--ql-search
        `(default-and
           (todo)
           (not (todo "DONE"))
           (ancestors (project ,root)))
      :title (format "Project %s" root)
      :super-groups octopus-todo-super-groups)))

(defun octopus--remote-todo-list (remote)
  "Display a todo list of a project on REMOTE."
  (octopus--ql-search
      `(default-and (todo)
         (not (todo "DONE"))
         (ancestors (project-remote-property ,remote)))
    :title (format "Project %s" remote)
    :super-groups octopus-todo-super-groups))

;;;; Sparse trees

;;;###autoload
(defun octopus-sparse-tree ()
  "Show sparse trees of project roots."
  (interactive)
  (org-ql-sparse-tree
   (octopus--ql-expand
     '(any-project))))

;;;###autoload
(defun octopus-sparse-tree-with-todos ()
  "Show sparse trees of project roots with unfinished todos."
  (interactive)
  (org-ql-sparse-tree
   (octopus--ql-expand
     '(and (any-project)
           (descendants (and (todo)
                             (not (todo "DONE"))))))))

;;;###autoload
(defun octopus-sparse-tree-todos (kwd)
  "Show sparse trees of project roots with unfinished todos."
  (interactive (list (when current-prefix-arg
                       (completing-read "Todo keyword: "
                                        (-map #'car org-todo-kwd-alist)))))
  (org-ql-sparse-tree
   (octopus--ql-expand
     `(and (todo ,@(when kwd
                     (list kwd)))
           (not (todo "DONE"))
           (ancestors (any-project))))))

(provide 'octopus)
;;; octopus.el ends here
