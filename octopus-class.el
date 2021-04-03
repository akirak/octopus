;;; octopus-class.el --- Class definition(s) and methods for octopus -*- lexical-binding: t -*-

(require 'eieio)

;;;; Types and classes

(defclass octopus-org-project-class ()
  ((project-dir :initarg :project-dir
                :type (or null string))
   (project-remote :initarg :project-remote
                   :type (or null string))
   (project-identity :initarg :project-identity)
   (tags :initarg :tags
         :type list)
   (properties :initarg :properties
               :type list)
   (marker :initarg :marker
           :type marker)
   (timestamp-info :initarg :timestamp-info
                   :type octopus-timestamp-info)))

(defclass octopus-org-project-group-class ()
  ((group-type :initarg :group-type
               :type symbol)
   (projects :initarg :projects
             :type list)
   (frecency-score
     :initarg :frecency-score
     :type number)))

;;;; Custom variables

(defcustom octopus-project-org-properties nil
  "List of properties to be included scanned in `octopus-switch-project'."
  :type '(repeat string))

(defcustom octopus-org-project-actions
  '((browse-dir
     :description "Browse the project directory"
     :slot project-dir
     :reduce (lambda (dirs)
               (octopus--pick-interactively "Project directory: "
                 (-uniq (-non-nil dirs))))
     :dispatch
     (lambda (dir)
       (cond
        ((and dir (file-directory-p dir))
         (octopus--browse-dir dir))
        (dir
         (error "%s is not an existing directory, so you cannot open it" dir))
        (t
         (error "Directory is nil")))))
    (display-org-marker
     :description "Display the Org subtree"
     :slot marker
     :reduce
     (lambda (markers)
       (octopus--single-or markers
         (octopus--select-org-marker
          "Select a subtree to display: " markers
          :name "Org subtrees for the project")))
     :dispatch octopus--display-org-marker)
    (todo-list
     :description "Show a todo list"
     :slot project-dir
     :reduce
     (lambda (dirs)
       (octopus--pick-interactively "Project directory: "
         (-uniq (-non-nil dirs))))
     :dispatch
     (lambda (dir)
       (if (and dir (file-directory-p dir))
           (octopus--project-todo-list dir)
         (error "Directory %s is nil or does not exist" dir)))))
  "Alist of actions.")

;;;; Search

(cl-defun octopus-org-project-groups (predicate
                                      &key
                                      (frecency t)
                                      (sort 'frecency))
  "Return a list of `octopus-org-project-group-class'.

PREDICATE is a QL query.

If FRECENCY is non-nil, scan all inactive timestamps in each
subtree and add the frecency score.

The result is sorted by SORT. By default, it is sorted by frecency."
  (--> (octopus--ql-select predicate
         :action `(octopus--org-project-from-subtree
                      :tags t
                      :properties t
                      :timestamps ,frecency))
    (octopus--group-org-projects 'dir it
                                 :frecency frecency)
    (octopus--sort-project-groups it sort)))

(defun octopus--sort-project-groups (groups key)
  "Sort GROUPS by KEY.

The groups must be a list of `octopus-org-project-group-class'.

KEY can be frecency or nil."
  (cl-ecase key
    (frecency (-sort (-on #'> (lambda (x)
                                (oref x frecency-score)))
                     groups))
    (nil groups)))

(cl-defun octopus--org-project-from-subtree (&key tags
                                                  properties
                                                  timestamps)
  (declare (indent 1))
  (make-instance 'octopus-org-project-class
                 :project-dir (octopus--org-project-dir)
                 :project-remote (octopus--org-project-remote)
                 :marker (point-marker)
                 :tags (when tags
                         (org-get-tags))
                 :properties (when properties
                               (--map (cons it
                                            (org-entry-get nil it t))
                                      octopus-project-org-properties))
                 :timestamp-info (when timestamps
                                   (octopus--subtree-timestamp-info))))

(cl-defun octopus--group-org-projects (type projects &key frecency)
  (->> projects
       (-group-by (lambda (x)
                    (cl-ecase type
                      (dir (or (oref x project-dir)
                               (oref x project-remote)))
                      (remote (or (oref x project-remote)
                                  (oref x project-dir))))))
       (-map (pcase-lambda (`(,_ . ,projects))
               (make-instance 'octopus-org-project-group-class
                              :group-type type
                              :projects projects
                              :frecency-score
                              (when frecency
                                (octopus-timestamp-info-frecency
                                 (-reduce #'octopus-merge-timestamp-info
                                          (--map (slot-value it 'timestamp-info)
                                                 projects)))))))))

;;;; Invoking actions

(defun octopus--run-action (action project)
  "Invoke an ACTION on PROJECT.

ACTION should be a symbol in `octopus-org-project-actions'."
  (let* ((plist (or (alist-get action octopus-org-project-actions)
                    (error "Undefined entry %s in octopus-org-project-actions" action)))
         (slot (plist-get plist :slot))
         (data (cl-etypecase project
                 (octopus-org-project-class
                  (slot-value project slot))
                 (octopus-org-project-group-class
                  (->> (oref project projects)
                       (-map (lambda (project) (slot-value project slot)))
                       (funcall (plist-get plist :reduce)))))))
    (funcall (plist-get plist :dispatch) data)))

(provide 'octopus-class)
;;; octopus-class.el ends here
