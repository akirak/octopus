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
  "Whether to enable contextual operations in some commands.

When this option is t, several commands in this package behave
differently depending on the major mode. That is, if the major
mode of the current buffer is `org-mode', it acts as if you are
inside the project repository refererred in the current project
Org tree.

These commands include:

- `octopus-project-org-root'
- `octopus-todo-list'"
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
          (windows (-map #'window-buffer (window-list)))
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

If ALL is non-nil or a prefix argument is given, the user can
choose a subtree of any project, not limited to the current one.

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
                         (->> (octopus--ql-select `(project ,(octopus--project-root))
                                ;; Return elements for sorting
                                :action 'element-with-markers
                                :sort #'octopus--dir-element-first)
                              (--map (or (org-element-property :org-hd-marker it)
                                         (org-element-property :org-marker it)))))
                    (octopus--single-or it
                      (octopus--select-org-marker
                       "Select a subtree to display: " it
                       :name "Org subtrees for the project")
                      "No subtree for the project"))))
      (if (or interactive (called-interactively-p 'any))
          (octopus--display-org-marker marker)
        marker))))

;;;; Find a local repository for the Org tree

;;;###autoload
(defun octopus-project-dir (&optional interactive)
  "Display the root directory of a project associated with the Org context.

This function looks for an Org property about a project from the
current Org entry and its ancestors and visit its root directory.

If INTERACTIVE, the function displays the root directory using
`octopus-browse-dir-fn'. Otherwise, it returns the directory."
  (interactive)
  (if-let (root (octopus--org-project-root))
      (if (or interactive (called-interactively-p 'any))
          (octopus--browse-dir root)
        root)
    (user-error "No property for the project identity")))

(defun octopus--org-project-root ()
  "Get the root directory of the Org context, possibly with a remote."
  (cond
   ((derived-mode-p 'org-mode)
    (assert (not (org-before-first-heading-p)))
    (or (octopus--org-project-dir)
        (-some->> (octopus--org-project-remote)
          (octopus--find-repository-by-remote-url))))
   ((derived-mode-p 'org-agenda-mode)
    (if-let (marker (org-agenda-get-any-marker))
        (org-with-point-at marker
          (octopus--org-project-root))
      (error "Cannot find an Org marker at point")))))

(defun octopus--find-repository-by-remote-url (remote-repo)
  "Find a local repository matching REMOTE-REPO."
  (--> (-filter (lambda (root)
                  (string-equal remote-repo
                                (octopus--abbreviate-remote-url root)))
                (octopus--repo-list))
    (octopus--single-or it
      (completing-read "Local copy: " it)
      (format "No local copy of the repository %s" remote-repo))))

;;;; Switching to a project

(defstruct octopus-project-dir-struct
  "Data type for representing projects with meta information."
  dir org-tags exists remote frecency-score markers last-ts-unix properties)

(defcustom octopus-project-dir-group 'dir
  "Field used to group project directories."
  :type '(choice (const dir)
                 (const remote)))

(defcustom octopus-project-org-properties nil
  "List of properties to be included scanned in `octopus-switch-project'."
  :type '(repeat string))

(cl-defun octopus--project-dirs (&key predicate)
  "Return a list of `octopus-project-dir-struct' objects from the environment.

PREDICATE is the same as in `octopus-switch-project'."
  (->> (octopus--ql-select (if predicate
                               `(and (any-project)
                                     ,predicate)
                             '(any-project))
         :action '(append `((dir . ,(octopus--org-project-dir))
                            (org-tags . ,(org-get-tags))
                            (remote . ,(octopus--org-project-remote))
                            (properties . ,(--map (cons it
                                                        (org-entry-get nil it t))
                                                  octopus-project-org-properties))
                            (marker . ,(point-marker)))
                          (octopus--subtree-timestamp-info)))
       (-group-by (lambda (x)
                    (cl-ecase octopus-project-dir-group
                      (dir (or (alist-get 'dir x)
                               (alist-get 'remote x)))
                      (remote (or (alist-get 'remote x)
                                  (alist-get 'dir x))))))
       (--map (let* ((alist (->> (cdr it)
                                 (-flatten-n 1)
                                 (-group-by #'car)
                                 (-map (lambda (cell)
                                         (cons (car cell)
                                               (->> (-map #'cdr (cdr cell))
                                                    (-flatten-n 1)
                                                    (-uniq)))))))
                     (remote (car (-non-nil (alist-get 'remote alist))))
                     (dir (car (-non-nil (alist-get 'dir alist))))
                     (org-tags (alist-get 'org-tags alist))
                     (markers (alist-get 'marker alist))
                     (properties (alist-get 'properties alist))
                     (timestamps (->> (alist-get 'last-ts alist)
                                      (-non-nil)
                                      (-map #'ts-unix)))
                     (last-ts (when timestamps
                                (-max timestamps)))
                     (ts-count (-sum (alist-get 'ts-count alist))))
                (make-octopus-project-dir-struct
                 :frecency-score
                 (if last-ts
                     (/ (* ts-count (octopus--frecency-timestamp-score last-ts))
                        (min ts-count 10))
                   0)
                 :dir dir
                 :org-tags org-tags
                 :exists (and dir (file-directory-p dir))
                 :markers markers
                 :last-ts-unix last-ts
                 :properties properties
                 :remote remote)))
       (-sort (-on #'> #'octopus-project-dir-struct-frecency-score))))

(defcustom octopus-switch-project-select-interface
  (if (require 'helm nil t)
      'helm
    #'completing-read)
  "Selection interface used in `octopus-switch-project'."
  :type '(choice (const :tag "Helm" helm)
                 function))

;;;###autoload
(cl-defun octopus-switch-project (&key predicate)
  "Switch to a project listed in Org files.

PREDICATE is an extra filter passed to `org-ql'."
  (interactive)
  ;; TODO: When a universal argument is given, sort projects by last inactive timestamp
  (let ((candidates (octopus--project-dirs)))
    (pcase octopus-switch-project-select-interface
      (`helm
       (helm-octopus-switch-project candidates))
      ((pred functionp)
       (octopus--browse-dir
        (funcall octopus-switch-project-select-interface
                 "Project root: "
                 (--map (or (octopus-project-dir-struct-dir it)
                            (octopus-project-dir-struct-remote it))
                        candidates))))
      (_
       (user-error "Invalid value for octopus-switch-project-select-interface: %s"
                   octopus-switch-project-select-interface)))))

;;;###autoload
(defun octopus-switch-project-by-org-category (category)
  "Switch to a project that belongs to a particular CATEGORY.

This command is experimental."
  (interactive (list (cond
                      (current-prefix-arg
                       (completing-read "Category: "
                                        (-uniq (octopus--ql-select '(project-dir-property)
                                                 :action '(org-get-category)))))
                      ((derived-mode-p 'org-mode)
                       (org-get-category))
                      (t
                       (user-error "Not in org-mode")))))
  (octopus-switch-project :predicate `(category ,category)))

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
  (unless (octopus--ql-select `(project ,root))
    (user-error "No Org tree found for project %s" root))
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
  (unless (octopus--ql-select `(project-remote-property ,remote))
    (user-error "No Org tree found for project remote %s" remote))
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
  "Show sparse trees of project todos.

With a prefix arg, this function displays todos with a particular
KWD."
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
