;;; octopus-org-ql.el --- Org-ql for octopus -*- lexical-binding: t -*-

(require 'octopus-utils)
(require 'octopus-org)

(require 'org-ql)

;;;; Custom variables

(defcustom octopus-org-ql-default-predicate
  '(not (tags "ARCHIVE"))
  "FIXME"
  :type 'sexp)

;;;; Completing-read like interface for Org markers

(defcustom octopus-org-marker-select-interface
  (if (require 'helm nil t)
      'helm
    #'completing-read)
  "FIXME"
  :type '(choice (const :tag "Helm" helm)
                 function))

(cl-defun octopus--user-select-org-marker (prompt markers &key name)
  (pcase octopus-org-marker-select-interface
    (`helm
     (octopus-helm-org-marker prompt markers
                              ((predicate functionp)
                               :name name))
     (->> (funcall octopus-org-marker-select-interface
                   prompt
                   (-map (lambda (marker)
                           (-> (org-with-point-at marker
                                 (funcall octopus-headline-format))
                               (propertize 'org-marker marker)))
                         markers))
          (get-char-property 0 'org-marker choice)))))

(defmacro octopus--single-or (items exp &optional null-message)
  (declare (indent 1))
  `(pcase ,items
     (`nil
      (error ,(or null-message "Empty list")))
     (`(,item)
      item)
     (_
      ,exp)))

;;;; Building queries

(defun octopus--ql-expand (exp)
  (declare (indent 0))
  (macroexpand-all
   exp
   '((default-and
       . (lambda (&rest args)
           (if octopus-org-ql-default-predicate
               `(and ,octopus-org-ql-default-predicate
                     ,@args)
             `(and ,@args))))
     (project-dir-property
      . (lambda (&rest args)
          `(property ,octopus-dir-property-name ,@args)))
     (project-remote-property
      . (lambda (&rest args)
          `(property ,octopus-remote-repo-property-name ,@args)))
     (any-project
      . (lambda ()
          '(or (project-dir-property)
               (project-remote-property))))
     (project
      . (lambda (root)
          `(and (project-dir-property ,(abbreviate-file-name
                                        (file-name-as-directory root)))
                ,@(when-let (remote (octopus--abbreviate-remote-url root))
                    (list `(project-remote-property ,remote)))))))))

;;;; Querying
;; Call `org-ql-select' to get a list of markers matching a certain condition.
(cl-defun octopus--ql-select (predicate &key (action #'point-marker) sort)
  (declare (indent 1))
  (org-ql-select (octopus-org-files)
    (octopus--ql-expand predicate)
    :action action
    :sort sort))

(cl-defun octopus--ql-search (predicate &key sort title super-groups)
  (declare (indent 1))
  (org-ql-search (octopus-org-files)
    (octopus--ql-expand predicate)
    :super-groups super-groups
    :title title
    :sort sort))

;;;; Compare functions for sorting

(defconst octopus-dir-property-symbol
  (intern (concat ":" octopus-dir-property-name)))
(defconst octopus-remote-repo-property-symbol
  (intern (concat ":" octopus-remote-repo-property-name)))

(defun octopus--dir-element-first (a b)
  "Compare two Org elements so entries with the directory property come first."
  (funcall (-on #'>
                (lambda (x)
                  (cond
                   ((org-element-property octopus-dir-property-symbol x)
                    2)
                   ((org-element-property octopus-remote-repo-property-symbol x)
                    1))))
           a b))

(defun octopus--collect-org-property-values (property)
  (->> (octopus--ql-select
           `(default-and (property ,property))
         :action `(org-entry-get nil property))
       (-non-nil)
       (-uniq)
       (-sort #'string<)))

(provide 'octopus-org-ql)
;;; octopus-org-ql.el ends here
