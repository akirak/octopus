;;; octopus-hydra.el --- Pretty hydra for octopus -*- lexical-binding: t -*-

;; Copyright (C) 2021 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "26.1"))
;; Keywords: convenience
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

;; This library provides `octopus-hydra' command, a pretty-hydra
;; interface to octopus.el.

;;; Code:

(require 'octopus)
(require 'pretty-hydra)

(defun octopus-hydra--org-title ()
  (let ((dir (octopus--org-project-dir))
        (remote (octopus--org-project-remote)))
    (if (or dir remote)
        (concat " Directory: " (or dir "-") "\n"
                " Remote:    " (or remote "-"))
      " Not in project")))

(defun octopus-hydra--project-title ()
  (let* ((dir (octopus--project-root))
         (remote (when dir (octopus--abbreviate-remote-url dir))))
    (if dir
        (concat " Project: " (or dir "-") "\n"
                " Remote:  " (or remote "-"))
      " Not in project")))

(pretty-hydra-define octopus-hydra
  (:title
   (concat "Octopus\n =======\n"
           (if (derived-mode-p 'org-mode)
               (octopus-hydra--org-title)
             (octopus-hydra--project-title)))
   :foreign-keys run
   :quit-key "Q")
  ("Show Org"
   (("r" octopus-project-org-root "Project subtree" :exit t)
    ("t" octopus-todo-list "Todo list" :exit t))
   "Org project"
   (("d" octopus-project-dir "Directory" :exit t)
    ("sd" octopus-org-set-project-dir "Set dir")
    ("sr" octopus-org-set-project-remote-repo "Set remote"))
   "Org sparse trees"
   (("or" octopus-sparse-tree "Projects")
    ("oa" octopus-sparse-tree-with-todos "Projects with todos")
    ("ot" octopus-sparse-tree-todos "Project todos"))
   "Capture"
   (("ct" octopus-capture-todo "Todo" :exit t)
    ("cc" octopus-capture-current-activity "Current task" :exit t)
    ("cp" octopus-capture-project "Project" :exit t))))

;;;###autoload
(defalias 'octopus-hydra #'octopus-hydra/body)

(provide 'octopus-hydra)
;;; octopus-hydra.el ends here
