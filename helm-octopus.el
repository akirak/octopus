;;; helm-octopus.el --- Helm interface for octopus -*- lexical-binding: t -*-

;; Copyright (C) 2021 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "26.1") (org "9.3") (helm "3.6") (octopus "0.1") (dash "2.18"))
;; Keywords: convenience tools outlines
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

;; This is a Helm interface to octopus.

;;; Code:

(require 'octopus-org)
(require 'octopus-utils)
(require 'octopus)
(require 'org)
(require 'helm)
(require 'dash)

(defgroup helm-octopus nil
  "Helm interface to octopus."
  :group 'octopus
  :group 'helm)

;;;; Custom variables

(defcustom helm-octopus-project-dir-format-fn
  #'helm-octopus-format-project-dir-struct-1
  "Function used to format `octopus-project-dir-struct' objects in Helm.

It should return a string."
  :type 'function)

(defcustom helm-octopus-extra-dir-fields
  '(helm-octopus-format-dir-org-tags)
  "Extra fields in `helm-octopus-format-project-dir-struct-1'.

This value must be a list of function that takes
`octopus-project-dir-struct' as the argument and returns a string
or nil."
  :type '(repeat function))

(defcustom helm-octopus-excluded-org-tags
  '("ORDERED" "noexport")
  "List of tags that are not displayed in Helm."
  :type '(repeat string))

;;;; Faces
(defface helm-octopus-remote-face
  '((t (:inherit font-lock-constant-face)))
  "Face for remote repository URLs."
  :group 'helm-octopus)

(defface helm-octopus-delimiter-face
  '((t (:inherit font-lock-type-face)))
  "Face for delimiters."
  :group 'helm-octopus)

(defface helm-octopus-directory-face
  '((t (:inherit font-lock-string-face)))
  "Face for existing directory paths."
  :group 'helm-octopus)

(defface helm-octopus-nonexistent-directory-face
  '((t (:inherit font-lock-comment-face)))
  "Face for non-existent directory paths."
  :group 'helm-octopus)

(defface helm-octopus-time-face
  '((t (:inherit font-lock-doc-face)))
  "Face for time strings."
  :group 'helm-octopus)

(defface helm-octopus-tag-face
  '((t (:inherit org-tag)))
  "Face for Org tags."
  :group 'helm-octopus)

;;;; Org markers

(defun helm-octopus-show-marker (marker)
  "Show an Org MARKER and narrow to it."
  (switch-to-buffer (marker-buffer marker))
  (widen)
  (goto-char marker)
  (org-show-entry)
  (org-show-children)
  ;; Show the property drawer
  (save-excursion
    (when (re-search-forward org-property-drawer-re (org-entry-end-position) t)
      (org-cycle)))
  (org-narrow-to-subtree))

(cl-defun helm-octopus--org-marker-sync-source (name markers
                                                     &key action)
  "Build a sync Helm source for Org markers.

NAME will be the name of the Helm source.

It creates an interface that lets the user select an item from MARKERS.

`octopus-headline-format' is used to format each candidate.

You must specify the ACTION of the Helm source."
  (declare (indent 1))
  (helm-build-sync-source name
    :candidates
    (-map (lambda (marker)
            (cons (org-with-point-at marker
                    (funcall octopus-headline-format))
                  marker))
          markers)
    :persistent-action #'helm-octopus-show-marker
    :action action))

;;;###autoload
(cl-defun helm-octopus-org-marker (prompt markers &key name)
  "Let the user select an Org marker via Helm.

PROMPT is the prompt of the minibuffer.

It lets the user select one of MARKERS.

NAME will be the name of the Helm sync source."
  (helm :prompt prompt
        :sources
        (helm-octopus--org-marker-sync-source name
          markers :action #'identity)))

;;;; Project directories

(defun helm-octopus-browse-dir (x)
  "Helm action for browsing a project directory.

X must be an instance of `octopus-project-dir-struct'."
  (octopus--browse-dir (octopus-project-dir-struct-dir x)))

(defun helm-octopus-display-marker (x)
  "Helm action for displaying an Org marker.

X must be an instance of `octopus-project-dir-struct'."
  (let ((markers (octopus-project-dir-struct-markers x)))
    (octopus--display-org-marker
     (octopus--single-or markers
       (octopus--select-org-marker
        "Select a subtree to display: " markers
        :name "Org subtrees for the project")))))

(defcustom helm-octopus-directory-persistent-action
  #'helm-octopus-display-marker
  "Persistent action of `helm-octopus-switch-project'."
  :type 'function)

(defcustom helm-octopus-directory-actions
  (list (cons "Browse"
              (-compose #'octopus--browse-dir
                        #'octopus-project-dir-struct-dir))
        (cons "Navigate to the Org marker"
              #'helm-octopus-display-marker))
  "List of actions to be available in `helm-octopus-switch-project'."
  :type 'alist)

;;;###autoload
(defun helm-octopus-switch-project (candidates)
  "Switch to a project directory.

CANDIDATES must be a list of `octopus-project-dir-struct' instances."
  (helm :project "Switch to a project: "
        :sources (helm-build-sync-source "Projects"
                   :multiline t
                   :candidates
                   (--map (cons (funcall helm-octopus-project-dir-format-fn it)
                                it)
                          candidates)
                   :persistent-action helm-octopus-directory-persistent-action
                   :action helm-octopus-directory-actions)))

(defun helm-octopus-format-project-dir-struct-1 (x)
  "Format a directory for Helm.

X must be an instance of `octopus-project-dir-struct'."
  (let ((remote (octopus-project-dir-struct-remote x))
        (dir (octopus-project-dir-struct-dir x))
        (time (octopus-project-dir-struct-last-ts-unix x))
        (extras (-non-nil (--map (funcall it x) helm-octopus-extra-dir-fields))))
    (->> (list (when remote
                 (propertize remote 'face 'helm-octopus-remote-face))
               (when (and remote dir)
                 (propertize " | " 'face 'helm-octopus-delimiter-face))
               (when dir
                 (propertize dir 'face (if (octopus-project-dir-struct-exists x)
                                           'helm-octopus-directory-face
                                         'helm-octopus-nonexistent-directory-face)))
               "  "
               (when time
                 (propertize (octopus--format-time time)
                             'face 'helm-octopus-time-face))
               (when extras
                 "\n  ")
               (string-join extras (propertize " | " 'face 'helm-octopus-delimiter-face)))
         (-non-nil)
         (string-join))))

;;;;; Format particular fields

(defun helm-octopus-format-dir-org-tags (x)
  "Format Org tags of X.

X must be a `octopus-project-dir-struct' object."
  (when-let (tags (--filter (not (member it helm-octopus-excluded-org-tags))
                            (octopus-project-dir-struct-org-tags x)))
    (propertize (string-join tags " ")
                'face 'helm-octopus-tag-face)))

(defun helm-octopus-format-dir-org-property (property x)
  "Extract an Org property.

PROPERTY is a string.

X must be a `octopus-project-dir-struct' object.

You can use `-partial' to build a function that extracts a
particular property, e.g.

  (-partial #'helm-octopus-format-dir-org-property \"category\")"
  (cdr (assoc property (octopus-project-dir-struct-properties x))))

(provide 'helm-octopus)
;;; helm-octopus.el ends here
