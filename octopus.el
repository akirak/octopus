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
(require 'octopus-class)
(require 'octopus-select)
(require 'octopus-utils)

(declare-function helm-octopus-project "ext:helm-octopus")

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
  (lambda (dir)
    (magit-status (locate-dominating-file dir ".git")))
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

;;;; Project files

;;;###autoload
(defun octopus-insert-file-link ()
  "Insert a link to a file in the project."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in org-mode"))
  (if-let* ((root (octopus--org-project-root))
            (default-directory root)
            (project (project-current))
            (files (project-files project))
            (file (completing-read (format "Insert file link (in %s): " root)
                                   (--map (file-relative-name it root)
                                          files)))
            (default-directory (file-name-directory
                                (buffer-file-name))))
      (org-insert-link nil (concat "file:" (expand-file-name file root))
                       file)
    (user-error "Aborted or no data")))

;;;; Switching to a project

(defcustom octopus-project-dir-group 'dir
  "Field used to group project directories."
  :type '(choice (const dir)
                 (const remote)))

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
  (let ((p (if predicate
               `(and (any-project)
                     ,predicate)
             '(any-project))))
    (pcase octopus-switch-project-select-interface
      (`helm
       (helm-octopus-project :predicate p))
      ((pred functionp)
       (octopus--browse-dir
        (funcall octopus-switch-project-select-interface
                 "Project root: "
                 (->> (octopus-org-project-groups p)
                      (-map (lambda (group)
                              (let ((project (car (oref group projects))))
                                (or (oref project project-dir)
                                    (oref project project-remote)))))))))
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

;;;###autoload
(defun octopus-org-set-project-clone-destination ()
  "Set the property for the clone destination in Org."
  (interactive)
  (octopus--org-put-property-from-exp-once octopus-clone-destination-property-name
    (completing-read "Clone destination: "
                     (octopus--uniq-files
                      (octopus--session-values
                       (car (octopus--git-worktrees default-directory)))))))

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
(defun octopus-sparse-tree-parents ()
  "Show sparse trees of project parents."
  (interactive)
  (org-ql-sparse-tree
   (octopus--ql-expand
     '(and (children (any-project))
           (not (any-project))))))

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

;;;; Generic actions

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

(cl-defun octopus--run-action (action project &key dispatch)
  "Invoke an ACTION on PROJECT.

ACTION should be a symbol in `octopus-org-project-actions'.

DISPATCH can be a function that takes the data as an
argument. This is intended for testing. ."
  (let ((plist (cl-etypecase action
                 (list action)
                 (symbol (or (alist-get action octopus-org-project-actions)
                             (error "Undefined entry %s in octopus-org-project-actions" action))))))
    (cl-labels
        ((reduce-data (slot xs)
                      ;; If an empty list is given, the result will be nil,
                      ;; so you can handle fallback situations.
                      (pcase (-uniq (-flatten-n 1 (-non-nil xs)))
                        (`(,item) item)
                        ;; TODO: Add support for fallback
                        (`nil (cl-ecase slot
                                (project-dir
                                 (octopus--find-repository-by-remote-url
                                  (get-data 'project-remote)))
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
                                :name "Org subtrees for the project"))))))
         (get-data (slot)
                   (cl-etypecase project
                     (octopus-org-project-class
                      (slot-value project slot))
                     (octopus-org-project-group-class
                      (->> (oref project projects)
                        (-map (lambda (project) (slot-value project slot)))
                        (reduce-data slot))))))
      (let* ((slot (plist-get plist :slot))
             (data (get-data slot)))
        (when-let (verify (plist-get plist :verify))
          (unless (funcall verify data)
            (error "Test failed on the value of %s: %s returned non-nil on %s"
                   slot verify data)))
        (funcall (or dispatch (plist-get plist :dispatch)) data)))))

;;;###autoload
(defun octopus-register-project (root)
  "Register a project to the current Org tree.

This function interactively creates an Org tree for a project
into the Org entry at point.

ROOT is the root directory of the project."
  (interactive (list (->> (completing-read "Select a project: "
                                           (octopus--uniq-files
                                            (octopus--session-values (octopus--project-root))))
                       (read-directory-name "Root directory of the project: ")
                       (abbreviate-file-name))))
  (unless (and (derived-mode-p 'org-mode)
               (not (org-before-first-heading-p)))
    (user-error "Run this in org-mode"))
  (let* ((vc-root (octopus--vc-root-dir root))
         (remote (read-string "Remote URL: "
                              (octopus--abbreviate-remote-url root)))
         (remote (unless (string-empty-p remote)
                   remote))
         (worktrees (when vc-root
                      (octopus--git-worktrees vc-root)))
         (clone-dest (car worktrees)))
    (if (file-equal-p root vc-root)
        (progn
          (when (or (octopus--org-project-remote)
                    (octopus--org-project-dir))
            (user-error "Already inside a project, so you cannot register a new project here"))
          (octopus--capture-project
           (save-excursion
             (org-back-to-heading)
             (point-marker))
           :root root
           :remote remote
           :clone-dest clone-dest))
      ;; TODO: Complex project
      )))

(cl-defun octopus--capture-project (marker &key root remote clone-dest
                                           immediate-finish)
  "Create an Org subtree for a project.

MARKER is the marker of an Org entry in which the project subtree
should be created.

ROOT, REMOTE, and CLONE-DEST are optional. ROOT is the root
directory, REMOTE is a remote repository, and CLONE-DEST is a
local directory into which the repository should be cloned.

This function uses `org-capture' to interactively create an Org
entry. However, if IMMEDIATE-FINISH is non-nil, the capture
session finishes immediately."
  (let ((org-capture-entry
         (list "_" "octopus-project"
               'entry
               (list 'function
                     `(lambda () (org-goto-marker-or-bmk ,marker)))
               (concat "* "
                       (if immediate-finish
                           (or remote (abbreviate-file-name root))
                         "%?")
                       "\n:PROPERTIES:\n"
                       (--> (list (cons octopus-dir-property-name
                                        (abbreviate-file-name root))
                                  (cons octopus-remote-repo-property-name
                                        remote)
                                  (cons octopus-clone-destination-property-name
                                        clone-dest))
                         (-filter #'cdr it)
                         (mapconcat (pcase-lambda (`(,key . ,value))
                                      (format ":%s: %s" key value))
                                    it "\n"))
                       "\n:END:")
               :immediate-finish immediate-finish)))
    (org-capture)))

(provide 'octopus)
;;; octopus.el ends here
