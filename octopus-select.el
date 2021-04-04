;;; octopus-select.el --- Select interface -*- lexical-binding: t -*-

(defgroup octopus-face
  nil
  "Faces for Octopus."
  :group 'octopus)

(defcustom octopus-select-project-extra-lines-format
  #'octopus-select-format-extra-lines-1
  ""
  :type 'function)

(defcustom helm-octopus-excluded-org-tags
  '("ORDERED" "noexport")
  "List of tags that are not displayed in Helm."
  :type '(repeat string))

;;;; Faces
(defface octopus-select-remote-face
  '((t (:inherit font-lock-constant-face)))
  "Face for remote repository URLs."
  :group 'octopus-face)

(defface octopus-remote-delimiter-face
  '((t (:inherit font-lock-type-face)))
  "Face for delimiters."
  :group 'octopus-face)

(defface octopus-remote-directory-face
  '((t (:inherit font-lock-string-face)))
  "Face for existing directory paths."
  :group 'octopus-face)

(defface octopus-remote-nonexistent-directory-face
  '((t (:inherit font-lock-comment-face)))
  "Face for non-existent directory paths."
  :group 'octopus-face)

(defface octopus-remote-time-face
  '((t (:inherit font-lock-doc-face)))
  "Face for time strings."
  :group 'octopus-face)

(defface octopus-remote-tag-face
  '((t (:inherit org-tag)))
  "Face for Org tags."
  :group 'octopus-face)

;;;; Formatting functions

;;;;; Generics

(cl-defgeneric octopus-format-candidate (x))

(cl-defgeneric octopus-format-candidate-multiline (x)
  (octopus-format-candidate x))

(cl-defmethod octopus-format-candidate-multiline ((x octopus-org-project-group-class))
  (let* ((projects (oref x projects))
         (project (car projects))
         (remote (oref project project-remote))
         (dir (oref project project-dir))
         (time (->> projects
                    (-map (lambda (project)
                            (when-let* ((info (oref project timestamp-info))
                                        (last-ts (octopus-timestamp-info-last-ts info)))
                              (ts-unix last-ts))))
                    (-non-nil)
                    (-max)))
         (markers (--map (slot-value it 'marker) projects))
         (extras (when octopus-select-project-extra-lines-format
                   (funcall octopus-select-project-extra-lines-format markers))))
    (octopus-select-format-project :remote remote
                                   :dir dir
                                   :time time
                                   :extras extras)))

;;;;; Formatting candidates

(cl-defun octopus-select-format-project (&key remote dir time extras)
  (->> (list (when remote
               (propertize remote 'face 'octopus-select-remote-face))
             (when (and remote dir)
               (propertize " | " 'face 'octopus-remote-delimiter-face))
             (when dir
               (propertize dir 'face (if (file-directory-p dir)
                                         'octopus-remote-directory-face
                                       'octopus-remote-nonexistent-directory-face)))
             "  "
             (when time
               (propertize (octopus--format-time time)
                           'face 'octopus-remote-time-face))
             (when extras
               "\n")
             extras)
       (-non-nil)
       (string-join)))

;;;;; Formatting particular fields

(defcustom octopus-select-hidden-tags
  '("ARCHIVE")
  "List of tags to hide from the selection interface."
  :type '(repeat string))

(defcustom octopus-select-project-extra-fields
  '((language (org-entry-get nil "LANGUAGE" t)
              (lambda (languages)
                (string-join (-uniq languages) " ")))
    (tags (org-get-tags)
          (lambda (tags)
            (-some-> (-uniq (-non-nil tags))
              (-difference octopus-select-hidden-tags)
              (string-join " ")
              (propertize 'face 'org-tag)))))
  "")

(defun octopus-select-format-extra-lines-1 (markers)
  "Format Org tags of X."
  (let ((fields (->> markers
                     (--map (org-with-point-at it
                              (-map (pcase-lambda (`(,key ,exp . ,_))
                                      (cons key
                                            (eval exp)))
                                    octopus-select-project-extra-fields)))
                     (-flatten-n 1)
                     (-group-by #'car)
                     (-map (pcase-lambda (`(,key . ,entries))
                             (when-let (xs (-flatten-n 1 (-map #'cdr entries)))
                               (funcall (nth 2 (assoc key octopus-select-project-extra-fields))
                                        xs))))
                     (-non-nil))))
    (when fields
      (concat "  "
              (string-join fields (propertize " | " 'face 'octopus-remote-delimiter-face))))))

(provide 'octopus-select)
;;; octopus-select.el ends here
