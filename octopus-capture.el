;;; octopus-capture.el --- Org-capture commands for projects and project tasks -*- lexical-binding: t -*-

(require 'octopus)

(defcustom octopus-capture-timestamp t
  ""
  :type 'boolean)

(cl-defun octopus-entry-capture-template (&key todo
                                               title
                                               tag-prompt
                                               body)
  (concat "* " (if todo (concat todo " ") "") (or title "%?") (if tag-prompt
                                                                  " %^g"
                                                                "")
          "\n"
          (if octopus-capture-timestamp
              ":PROPERTIES:\n:CREATED_TIME: %U\n:END:\n"
            "")
          (or body "")))

(defcustom octopus-default-todo-capture-template
  (octopus-entry-capture-template :todo "TODO"
                                  :title "%?"
                                  :tag-prompt t)
  ""
  :type 'string)

(defcustom octopus-todo-capture-options
  '(:clock-in t :clock-resume t)
  ""
  :type 'plist)

(defcustom octopus-alternative-todo-capture-template-alist
  `((started-with-input
     . ,(octopus-entry-capture-template
         :todo "STARTED"
         :title "%i"
         :body "%a\n\n%?")))
  ""
  :type 'string)

(defcustom octopus-project-capture-template
  (octopus-entry-capture-template :title "%?")
  ""
  :type 'string)

(defcustom octopus-project-capture-options
  '(:clock-in t :clock-resume t)
  ""
  :type 'plist)

(defun octopus--capture-entry-to-marker (marker template &rest props)
  (let ((org-capture-entry `("_" "octopus"
                             entry
                             (function (lambda () (org-goto-marker-or-bmk ,marker)))
                             ,template ,@props)))
    (org-capture)))

(defun octopus-create-project-subtree (&optional arg)
  (interactive "P")
  (pcase arg
    (`(16)
     (octopus-display-project-org-subtree t))
    (_
     (let ((marker (--> (octopus--ql-select (default-and (children (any-project)))
                                            :action '(prog1 (point-marker)
                                                       (org-end-of-subtree)))
                     (octopus--user-select-org-marker
                      "Project context: " parent-markers
                      :name "Parents of existing project subtrees"))))
       (apply #'octopus--capture-entry-to-marker
              marker
              octopus-project-capture-template
              octopus-project-capture-options)))))

(cl-defun octopus-goto-todo-location (&key root remote)
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
                   (octopus--user-select-org-marker
                    "Select a capture location: "
                    parents (format "Project %s" identity))
                   "Cannot find project destination")))
    (switch-to-buffer (marker-buffer parent))
    (widen)
    (goto-char parent)))

(defun octopus--capture-entry-to-todo-location (location-spec template
                                                              &rest props)
  (let ((org-capture-entry `("p" "Project todo"
                             entry (function
                                    (lambda () (octopus-goto-todo-location
                                                ,@location-spec)))
                             ,template ,@props)))
    (org-capture)))

;;;###autoload
(cl-defun octopus-capture-project-todo (&key template
                                             root
                                             remote)
  (interactive (list :remote (equal current-prefix-arg '(16))
                     :root (equal current-prefix-arg '(16))))
  (let ((location (cond
                   ((and root (not (stringp root)))
                    (list :root (octopus-select-project-dir-in-org)))
                   ((and remote (not (stringp remote)))
                    (list :remote (octopus-select-project-remote-repo-in-org)))
                   (t
                    (list :root root :remote remote))))
        (template-string (cl-typecase template
                           (null octopus-default-todo-capture-template)
                           (string template)
                           (symbol (or (alist-get template octopus-alternative-todo-capture-template-alist)
                                       (user-error "Cannot find a template named %s" template))))))
    (apply #'octopus--capture-entry-to-todo-location
           location template-string
           octopus-todo-capture-options)))

;; Provided as an example.
;;;###autoload
(defun octopus-start-project-todo-with-title (title)
  (interactive "sName of the task: ")
  (let ((org-capture-initial title))
    (octopus-capture-project-todo 'started-with-input)))

(provide 'octopus-capture)
;;; octopus-capture.el ends here
