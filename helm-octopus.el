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
(require 'octopus-class)
(require 'octopus-select)
(require 'octopus)
(require 'org)
(require 'helm)
(require 'dash)

(defgroup helm-octopus nil
  "Helm interface to octopus."
  :group 'octopus
  :group 'helm)

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

(defcustom helm-octopus-project-persistent-action
  'display-org-marker
  "Persistent action of `helm-octopus-project'.

This can be a symbol in `octopus-org-project-actions'."
  :type '(choice symbol null))

(defclass helm-octopus-project-source (helm-source-sync)
  ((multiline :initform t)))

;;;###autoload
(defun helm-octopus-project (predicate)
  "Switch to a project directory."
  (interactive (list '(any-project)))
  (helm :project "Switch to a project: "
        :sources (helm-make-source "Projects" 'helm-octopus-project-source
                   :candidates
                   (->> (octopus-org-project-groups predicate)
                        (-map (lambda (it)
                                (cons (octopus-format-candidate-multiline it)
                                      it))))
                   :persistent-action
                   (-partial #'octopus--run-action
                             helm-octopus-project-persistent-action)
                   :action
                   (-map (pcase-lambda (`(,symbol . ,plist))
                           (cons (plist-get plist :description)
                                 (-partial #'octopus--run-action symbol)))
                         octopus-org-project-actions))))

(provide 'helm-octopus)
;;; helm-octopus.el ends here
