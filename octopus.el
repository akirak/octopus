;;; octopus.el --- Integration between Org and project.el -*- lexical-binding: t -*-

(require 'octopus-utils)

(defcustom octopus-git-repository-list-function
  #'magit-list-repos
  "Function that returns a list of local Git repositories."
  :type 'function)

(defsubst octopus--repo-list ()
  (funcall octopus-git-repository-list-function))

;;;; Find an Org tree for the project

(defcustom octopus-display-org-buffer-function
  #'pop-to-buffer
  ;; #'switch-to-buffer-other-window
  "Function used to display an Org buffer."
  :type 'function)

(defcustom octopus-org-tree-show-function
  #'org-narrow-to-subtree
  "Function used to show the subtree at point."
  :type 'function)

(defun octopus--display-org-marker (marker)
  (with-current-buffer (marker-buffer marker)
    (funcall octopus-display-org-buffer-function (current-buffer))
    (widen)
    (goto-char marker)
    (when (boundp octopus-org-tree-show-function)
      (funcall octopus-org-tree-show-function))))

;;;###autoload
(defun octopus-display-project-org-subtree (&optional arg)
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

;;;; Set a project property

(defmacro octopus-select-from-other-windows (prompt exp)
  "Evaluate EXP in each live window and select a value different from the current one."
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
  "Evaluate EXP in each live window and select a value different from the current one."
  (declare (indent 1))
  `(octopus-select-from-other-windows ,prompt ,exp))

;;;###autoload
(defun octopus-org-set-project-dir ()
  (interactive)
  (octopus--org-put-new-property-with octopus-dir-property-name
    (octopus-select-from-session "Project directory: "
      (octopus--project-root))))

;;;###autoload
(defun octopus-org-set-project-remote-repo (&optional arg)
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
  (or (bound-and-true-p octopus-org-super-agenda-todo-groups)
      '((:todo "DONE" :order 5)
        (:auto-planning t)
        (:todo "STARTED")
        (:todo "WAITING")
        (:todo "REVIEW")
        (:todo "NEXT")
        (:todo "STOPPED")
        (:todo t)))
  "FIXME: Docstring")

(defun octopus-project-todo-list (arg)
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
  (let ((default-directory root))
    (octopus--ql-search
        `(default-and
           (todo)
           (not (todo "DONE"))
           (ancestors (project ,root)))
      :title (format "Project %s" root)
      :super-groups octopus-project-todo-super-groups)))

(defun octopus--project-todo-list-on-remote (remote)
  (octopus--ql-search
      `(default-and (todo)
         (not (todo "DONE"))
         (ancestors (project-remote-property ,remote)))
    :title (format "Project %s" remote)
    :super-groups octopus-project-todo-super-groups))

;;; Find a local repository for the Org tree

(defcustom octopus-browse-repository-function
  #'magit-status
  "Function used to open a repository."
  :type 'function)

(defsubst octopus--browse-repository (dir)
  (funcall octopus-browse-repository-function dir))

;;;###autoload
(defun octopus-project-local-repository ()
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Must be run in org-mode"))
  (assert (not (org-before-first-heading-p)))
  (if-let (root (octopus--org-project-dir))
      (octopus--browse-repository root)
    (if-let (remote-repo (octopus--org-project-remote))
        (pcase (->> (octopus--repo-list)
                    (-filter (lambda (root)
                               (string-equal remote-repo
                                             (octopus--abbreviate-remote-url root)))))
          (`(,x)
           (octopus--browse-repository x))
          (`nil
           (user-error "No local copy of the repository %s" remote-repo))
          (xs
           (octopus--browse-repository (completing-read "Local copy: " xs))))
      (user-error "No property for the project identity"))))

(provide 'octopus)
;;; octopus.el ends here
