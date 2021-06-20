;;; octopus-hierarchy.el --- Display projects using hierarchy.el -*- lexical-binding: t -*-

(require 'hierarchy)
(require 'octopus-org)
(require 'octopus-org-ql)

(defgroup octopus-hierarchy nil
  "Display projects using hierarchy.el."
  :group 'octopus)

(defcustom octopus-hierarchy-flat-contexts nil
  "Display flat context nodes under file trees.

This is applicable to In `octopus-file-hierarchy',"
  :group 'octopus-hierarchy
  :type 'boolean)

;;;; Data types

(cl-defstruct octopus-hierarchy-file-tree
  "Data type representing a file in `octoups-org-files'."
  filename)

(cl-defstruct octopus-hierarchy-context-tree
  "Data type representing an Org context containing projects."
  filename marker olp leaves)

(cl-defstruct octopus-hierarchy-project-tree
  "Data type representing an Org subtree for a project."
  marker remote directory)

;;;; Generic functions for hierarchy.el
(cl-defgeneric octopus--hierarchy-label (item level)
  "Insert the label of an ITEM at LEVEL.")

(cl-defgeneric octopus--hierarchy-parent (item)
  "Return the parent node of ITEM.

See the documentation on `hierarchy-add-tree'.")

(cl-defgeneric octopus--hierarchy-children (item)
  "Return the children of ITEM.

See the documentation on `hierarchy-add-tree'.")

(cl-defgeneric octopus--hierarchy-accept (item)
  "Return non-nil if ITEM should be an item of the hierarchy.

For details, see the section on acceptfn in the documentation on
`hierarchy-add-tree'."
  item)

(cl-defgeneric octopus--hierarchy-action (item level)
  "Dispatch an action on ITEM at LEVEL.

See the documentation on `hierarchy-labelfn-button'."
  (error "Undefined action on %s (at %d)." item level))

(cl-defgeneric octopus--hierarchy-marker (item)
  "Return a marker to ITEM."
  (error "Undefined on %s" item))

;;;; Methods
;;;;; octopus-hierarchy-file-tree

(cl-defmethod octopus--hierarchy-label ((x octopus-hierarchy-file-tree) _)
  (insert (file-name-nondirectory (octopus-hierarchy-file-tree-filename x))))

(cl-defmethod octopus--hierarchy-parent ((x octopus-hierarchy-file-tree))
  nil)

(cl-defmethod octopus--hierarchy-children ((x octopus-hierarchy-file-tree))
  (octopus--hierarchy-file-contexts (octopus-hierarchy-file-tree-filename x)))

(defun octopus--hierarchy-file-contexts (filename)
  (cl-labels
      ((map-olp
        (fn nodes)
        (--map (let ((olp (octopus-hierarchy-context-tree-olp it)))
                 (setf (octopus-hierarchy-context-tree-olp it)
                       (funcall fn (copy-sequence olp)))
                 it)
               nodes))
       (to-trees
        (nodes)
        (cl-check-type nodes sequence)
        (let ((olps (-map #'octopus-hierarchy-context-tree-olp nodes)))
          (if-let (cp (apply #'-common-prefix olps))
              (list (make-octopus-hierarchy-context-tree
                     :olp cp
                     :filename filename
                     :leaves
                     (let ((len (length cp)))
                       (to-trees (map-olp (lambda (olp) (-drop len olp)) nodes)))))
            (->> (-group-by (-compose #'car #'octopus-hierarchy-context-tree-olp) nodes)
              (-map (pcase-lambda (`(,step . ,group-entries))
                      (if (= 1 (length group-entries))
                          group-entries
                        ;; (if-let (direct (--find (null (octopus-hierarchy-context-tree-olp it))
                        ;;                         group-entries))
                        ;;     (progn
                        ;;       (setf (octopus-hierarchy-context-tree-olp direct)
                        ;;             (list step))
                        ;;       (setf (octopus-hierarchy-context-tree-leaves direct)
                        ;;             (to-trees (-remove-item direct group-entries)))
                        ;;       (list direct))
                        ;;   (let ((trees (to-trees (map-olp #'cdr group-entries))))
                        ;;     (if (= 1 (length trees))
                        ;;         (map-olp (lambda (olp) (cons step olp)) trees)
                        ;;       (list (make-octopus-hierarchy-context-tree
                        ;;              :olp (list step)
                        ;;              :filename filename
                        ;;              :leaves trees)))))
                        (let ((trees (to-trees (map-olp #'cdr group-entries))))
                          (if (= 1 (length trees))
                              (map-olp (lambda (olp) (cons step olp)) trees)
                            (list (make-octopus-hierarchy-context-tree
                                   :olp (list step)
                                   :filename filename
                                   :leaves trees)))))))
              (-flatten-n 1)
              (-filter #'octopus-hierarchy-context-tree-olp))))))
    (funcall (if octopus-hierarchy-flat-contexts
                 #'identity
               #'to-trees)
             (with-current-buffer (or (find-buffer-visiting filename)
                                      (find-file-noselect filename))
               (org-save-outline-visibility t
                 (org-with-wide-buffer
                  (org-ql-select (current-buffer)
                    (octopus--ql-expand '(and (children (any-project))
                                              (not (or (any-project)
                                                       (ancestors (any-project))))))
                    :action
                    `(make-octopus-hierarchy-context-tree
                      :filename ,filename
                      :marker (point-marker)
                      :olp (org-get-outline-path t)
                      :leaves nil))))))))

(cl-defmethod octopus--hierarchy-accept ((x octopus-hierarchy-file-tree))
  t)

(cl-defmethod octopus--hierarchy-action ((file octopus-hierarchy-file-tree) _)
  "Dispatch an action on FILE."
  (find-file file))

;;;;; octopus-hierarchy-context-tree

(cl-defmethod octopus--hierarchy-label ((x octopus-hierarchy-context-tree) _)
  (insert (substring-no-properties
           (org-format-outline-path
            (octopus-hierarchy-context-tree-olp x)))))

(cl-defmethod octopus--hierarchy-parent ((x octopus-hierarchy-context-tree))
  nil)

(cl-defmethod octopus--hierarchy-children ((x octopus-hierarchy-context-tree))
  (append (octopus-hierarchy-context-tree-leaves x)
          ;; (org-with-point-at (or (octopus--hierarchy-marker x)
          ;;                        (error "No marker on %s"
          ;;                               (octopus-hierarchy-context-tree-olp x)))
          ;;   (let ((subtree-end (save-excursion
          ;;                        (org-end-of-subtree)))
          ;;         (level (org-reduced-level (org-outline-level)))
          ;;         result)
          ;;     (while (re-search-forward org-heading-regexp subtree-end t)
          ;;       (when (and (= (org-reduced-level (org-outline-level))
          ;;                     (1+ level))
          ;;                  (or (octopus--org-project-remote)
          ;;                      (octopus--org-project-dir)))
          ;;         (push (make-octopus-hierarchy-project-tree
          ;;                :marker (point-marker)
          ;;                :remote (octopus--org-project-remote)
          ;;                :directory (octopus--org-project-dir))
          ;;               result)
          ;;         (org-end-of-subtree)))
          ;;     (nreverse result)))
          ))

(cl-defmethod octopus--hierarchy-action ((context octopus-hierarchy-context-tree) _)
  "Dispatch an action on CONTEXT."
  (octopus--display-org-marker (octopus--hierarchy-marker context)))

(cl-defmethod octopus--hierarchy-marker ((context octopus-hierarchy-context-tree))
  "Return an Org marker to CONTEXT."
  (or (octopus-hierarchy-context-tree-marker context)
      ;; (let* ((child (car (octopus-hierarchy-context-tree-leaves x)))
      ;;        (child-marker (octopus--hierarchy-marker child))
      ;;        (depth (length (octopus-hierarchy-context-tree-olp child))))
      ;;   (org-with-point-at child-marker
      ;;     (let* ((start-level (org-reduced-level (org-outline-level)))
      ;;            (marker (catch 'marker
      ;;                      (while (re-search-backward org-heading-regexp nil)
      ;;                        (when (= (org-reduced-level (org-outline-level))
      ;;                                 (- start-level depth))
      ;;                          (throw 'marker (point-marker)))))))
      ;;       ;; Cache the marker
      ;;       (setf (octopus-hierarchy-context-tree-marker context) marker)
      ;;       marker)))
      ))

;;;;; octopus-hierarchy-project-tree

(cl-defmethod octopus--hierarchy-label ((x octopus-hierarchy-project-tree) level)
  (insert (or (octopus-hierarchy-project-tree-remote x)
              (octopus-hierarchy-project-tree-directory x))))

(cl-defmethod octopus--hierarchy-parent ((x octopus-hierarchy-project-tree))
  nil)

(cl-defmethod octopus--hierarchy-children ((x octopus-hierarchy-project-tree))
  nil)

(cl-defmethod octopus--hierarchy-accept ((x octopus-hierarchy-project-tree))
  t)

(cl-defmethod octopus--hierarchy-action ((project octopus-hierarchy-project-tree) _)
  "Dispatch an action on PROJECT."
  (octopus--display-org-marker (octopus--hierarchy-marker project)))

(cl-defmethod octopus--hierarchy-marker ((project octopus-hierarchy-project-tree))
  "Return an Org marker to PROJECT."
  (octopus-hierarchy-project-tree-marker project))

;;;; Commands

(defun octopus--build-file-hierarchy ()
  "Display hierarchy of FOLDER in a tabulated list."
  (let ((hierarchy (hierarchy-new)))
    (hierarchy-add-trees hierarchy
                         (--map (make-octopus-hierarchy-file-tree :filename it)
                                (list (org-starter-locate-file "practice.org" nil t))
                                ;; (octopus-org-files)
                                )
                         #'octopus--hierarchy-parent
                         #'octopus--hierarchy-children)
    hierarchy))

(defun octopus-file-hierarchy ()
  "Display all major modes and their inheritance relationship."
  (interactive)
  (let* ((hierarchy (octopus--build-file-hierarchy))
         (buffer (hierarchy-tree-display
                  hierarchy
                  (hierarchy-labelfn-button
                   #'octopus--hierarchy-label
                   #'octopus--hierarchy-action))))
    (switch-to-buffer buffer)))

(provide 'octopus-hierarchy)
;;; 'octopus-hierarchy.el ends here
