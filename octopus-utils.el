;;; octopus-utils.el --- Utilities for octopus -*- lexical-binding: t -*-

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

;; This library provides utilities.

;;; Code:

(require 'dash)
(require 'project)

(defcustom octopus-default-git-remote-name "origin"
  "Name of the default Git remote."
  :type 'string
  :group 'octopus)

(defmacro octopus--single-or (items exp &optional null-message)
  "If necessary, pick an item from multiple candidates using a given expression.

If ITEMS contain only one element, return it.

If there are multiple items, evalate EXP to pick an item. The
expression should be a call to `completing-read'-like interface,
e.g. `octopus--select-org-marker'.

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

(cl-defun octopus--pick-interactively (prompt items
                                              &optional (format-fn #'identity))
  (declare (indent 1))
  (pcase items
    (`nil
     nil)
    (`(,item)
     item)
    (_
     (let ((result (completing-read
                    prompt
                    (-map (lambda (x)
                            (propertize (funcall format-fn x)
                                        'octopus-value x))
                          items))))
       (get-char-property 0 'octopus-value result)))))

(defun octopus--default-git-remote-url (&optional dir)
  "Return the URL of the default Git remote at DIR.

The remote name is specified by `octopus-default-git-remote-name'."
  (let ((default-directory (or dir default-directory)))
    (car (ignore-errors
           (octopus--get-git-config-local
            (format "remote.%s.url" octopus-default-git-remote-name))))))

(defun octopus--get-git-config-local (key)
  "Return the value of KEY from the local Git config."
  (-> (octopus--read-process "git" "config" "--local" "--get" key)
      (split-string "\n")
      (-non-nil)))

(defun octopus--read-process (program &rest args)
  "Return the standard output from PROGRAM run with ARGS."
  (with-temp-buffer
    (if (zerop (apply #'call-process program
                      nil (list (current-buffer) nil) nil
                      args))
        (buffer-string)
      (error "Process %s %s returned non-zero" program args))))

;; Regular expression pattern from git-identity.el
(eval-and-compile
  (defconst octopus--git-xalpha
    ;; TODO: Add thorough tests and fix this pattern
    (let* ((safe "-$=_@.&+")
           (extra "!*(),~")
           ;; I don't think people would want URLs containing
           ;; double/single quotes, but the spec contains them.
           ;;
           ;; (extra "!*\"'(),")
           (escape '(and "%" (char hex) (char hex))))
      `(or ,escape (char alpha digit ,safe ,extra)))))

(defun octopus--flake-url (git-url)
  "Convert a GIT-URL to an input URL in Nix flakes."
  (save-match-data
    (cond
     ((string-match (rx bol "git@github.com:"
                        (group (* (and (+ (eval octopus--git-xalpha)) "/"))
                               (+? (eval octopus--git-xalpha)))
                        (?  ".git")
                        eol)
                    git-url)
      (concat "github:" (match-string 1 git-url)))
     ((string-match (rx bol "https://github.com/"
                        (group (+ (eval octopus--git-xalpha))
                               "/"
                               (+? (eval octopus--git-xalpha)))
                        (or (and (?  ".git")
                                 (?  "/")
                                 eol)
                            (and "/blob/"
                                 (group (+ (eval octopus--git-xalpha)))
                                 "/")))
                    git-url)
      (concat "github:" (match-string 1 git-url)
              (-some->> (match-string 2 git-url)
                (concat "/"))))
     (t
      git-url))))

(defun octopus--abbreviate-remote-url (dir)
  "Return the flake URL of the default remote at DIR."
  (when-let (url (octopus--default-git-remote-url dir))
    (octopus--flake-url url)))

(defsubst octopus--project-root (&optional maybe-prompt)
  "Return the project root at the default directory.

This is just a combination `project-root' and `project-current'
which takes MAYBE-PROMPT as an argument, which see."
  (when-let (current (project-current maybe-prompt))
    (project-root current)))

(defun octopus--frecency-timestamp-score (unix)
  "Calculate the time score of the given UNIX time."
  (let ((secs (- (float-time) unix)))
    (cond
     ((<= secs 14400)
      100)
     ((<= secs 86400)
      80)
     ((<= secs 259200)
      60)
     ((<= secs 604800)
      40)
     ((<= secs 2419200)
      20)
     ((<= secs 7776000)
      10)
     (t
      0))))

(defun octopus--format-time (time)
  "Format TIME for human."
  (let ((diff (- (float-time) time)))
    (if (< diff (* 3600 48))
        (octopus--format-duration diff)
      (format-time-string "%F (%a)" time))))

(defun octopus--format-duration (seconds)
  "Format a time duration in SECONDS."
  (cond
   ((< seconds 120)
    "just now")
   ((< seconds 3600)
    (format "%.f minutes ago" (/ seconds 60)))
   ((< seconds (* 3600 24))
    (format "%.f hours ago" (/ seconds 3600)))
   ((< seconds (* 3600 24))
    (format "%.f hours ago" (/ seconds 3600)))
   ((< seconds (* 3600 24 60))
    (format "%.f days ago" (/ seconds (* 3600 24))))
   ((< seconds (* 3600 24 365))
    (format "%.f months ago" (/ seconds (* 3600 24 30))))
   (t
    (format "%.f years ago" (/ seconds (* 3600 24 365))))))

(provide 'octopus-utils)
;;; octopus-utils.el ends here
