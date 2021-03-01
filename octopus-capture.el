;;; octopus-capture.el --- Org-capture commands for projects and project tasks -*- lexical-binding: t -*-

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

;; This library provides `org-capture' wrapper commands using octopus.el.

;; Note these features are experimental.

;;; Code:

(require 'octopus)
(require 'octopus-org-ql)

(defcustom octopus-capture-timestamp t
  "Whether to include a timestamp in capture templates.

When this option is set to t, `octopus-entry-capture-template'
returns a templates that contains CREATED_TIME property which
tracks the creation time of each created entry.

The function is used across this library, so it will affect most
of the templates. If you customize all of the templates without
using the function, this option will be ineffective."
  :type 'boolean)

(cl-defun octopus-entry-capture-template (&key todo
                                               heading
                                               tag-prompt
                                               body)
  "Returns a template string from the given arguments.

TODO is a todo state of the created entry.

HEADING will be the heading of the generated template.

If TAG-PROMPT is non-nil, the user will be asked for a tag.

BODY is the entry body.

See `org-capture-templates' for the syntax."
  (concat "* " (if todo (concat todo " ") "") (or heading "%?")
          (if tag-prompt
              " %^g"
            "")
          "\n"
          (if octopus-capture-timestamp
              ":PROPERTIES:\n:CREATED_TIME: %U\n:END:\n"
            "")
          (or body "")))

(defcustom octopus-capture-template-alist
  `((todo
     ,(octopus-entry-capture-template :todo "TODO"
                                      :heading "%?"
                                      :tag-prompt t))
    (project
     ,(octopus-entry-capture-template :heading "%?"))
    (current-from-title
     ,(octopus-entry-capture-template
       :todo "STARTED"
       :heading "%i"
       :body "%a\n\n%?")
     :clock-in t :clock-resume t))
  "Alist of todo capture templates."
  :type '(alist :key-type symbol
                :value-type (cons string plist)))

(defun octopus--capture-entry-to-marker (marker template &rest props)
  "Capture an entry to a given marker.

This creates a new Org entry into MARKER using `org-capture'.

TEMPLATE is the template body which must be a string, and PROPS
is a plist as in each entry in `org-capture-templates'."
  (let ((org-capture-entry `("_" "octopus"
                             entry
                             (function (lambda ()
                                         (org-goto-marker-or-bmk ,marker)))
                             ,template ,@props)))
    (org-capture)))

;;;###autoload
(defun octopus-capture-project (&optional arg)
  "Create an Org subtree for the current project.

If two universal prefixes are given as ARG, it displays a project
subtree instead."
  (interactive "P")
  (pcase arg
    (`(16)
     (octopus-project-org-root t))
    (_
     (let ((marker (--> (octopus--ql-select '(default-and (children (any-project)))
                          :action '(prog1 (point-marker)
                                     (org-end-of-subtree)))
                     (octopus--select-org-marker
                      "Project context: " it
                      :name "Parents of existing project subtrees"))))
       (apply #'octopus--capture-entry-to-marker
              marker
              (alist-get 'project octopus-capture-template-alist))))))

(cl-defun octopus-todo-capture-location (&key root remote)
  "Go to a location in which todo entries should be created.

Either ROOT or REMOTE should be given. The former is the root
directory of a project, and the latter is the remote repository
URL.

This function is intended for internal use."
  (let* ((root (unless remote
                 (or root
                     (octopus--project-root))))
         (identity (or remote (abbreviate-file-name root)))
         (subtree-pred (if remote
                           `(project-remote-property ,remote)
                         `(project ,root)))
         (parents (or (octopus--ql-select
                          `(default-and (ancestors ,subtree-pred)
                             (property "PROJECT_CAPTURE_LOCATION"))
                        :action #'point-marker)
                      (octopus--ql-select
                          `(default-and (ancestors ,subtree-pred)
                             (children (todo)))
                        :action `(prog1 (point-marker)
                                   (org-end-of-subtree)))
                      ;; Otherwise, create todos directly below the project subtree
                      (octopus--ql-select
                          `(default-and ,subtree-pred)
                        :action #'point-marker)))
         (parent (octopus--single-or parents
                   (octopus--select-org-marker
                    "Select a capture location: "
                    parents
                    :name (format "Project %s" identity))
                   "Cannot find project destination")))
    (switch-to-buffer (marker-buffer parent))
    (widen)
    (goto-char parent)))

(defun octopus--capture-todo-entry (location-spec
                                    template
                                    &rest props)
  "Creates a project todo entry from the given arguments.

LOCATION-SPEC is a list of arguments passed to
`octopus-todo-capture-location'.

TEMPLATE and PROPS are arguments of an entry in `org-capture-templates'.
The former is a string, and the latter is a plist."
  (let ((org-capture-entry `("p" "Project todo"
                             entry (function
                                    (lambda () (octopus-todo-capture-location
                                                ,@location-spec)))
                             ,template ,@props)))
    (org-capture)))

;;;###autoload
(cl-defun octopus-capture-todo (&optional template
                                          &key
                                          props
                                          root
                                          remote)
  "Create a project todo using `org-capture'.

As an interactive function, the destination will be the current
project by default. Alternatively, you can choose the root
directory with one universal argument, or choose the remote
repository with two universal arguments.

As a non-interactive function, TEMPLATE is optional. It can be a
literal string of the template body or a symbol in
`octopus-capture-template-alist'.

ROOT and REMOTE are passed to `octopus-todo-capture-location'.
You should specify on of those."
  (interactive (list :remote (equal current-prefix-arg '(16))
                     :root (equal current-prefix-arg '(4))))
  (let ((location (cond
                   ((and root (not (stringp root)))
                    (list :root (octopus-select-project-dir-in-org)))
                   ((and remote (not (stringp remote)))
                    (list :remote (octopus-select-project-remote-repo-in-org)))
                   (t
                    (list :root root :remote remote)))))
    (apply #'octopus--capture-todo-entry
           location
           (cl-etypecase template
             (string (cons template props))
             (null (alist-get 'todo octopus-capture-template-alist))
             (symbol (or (alist-get template octopus-capture-template-alist)
                         (user-error "Cannot find a template named %s" template)))))))

;; Provided as an example.
;;;###autoload
(defun octopus-capture-current-activity (title)
  "Create a todo for the current project with a given TITLE."
  (interactive "sName of the task: ")
  (let ((org-capture-initial title))
    (octopus-capture-todo 'current-from-title)))

(provide 'octopus-capture)
;;; octopus-capture.el ends here
