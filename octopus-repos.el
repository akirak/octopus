;;; octopus-repos.el --- Repository listing -*- lexical-binding: t -*-

(defcustom octopus-repo-directory-alist
  '(("~/archives/commons/git/" :depth 3)
    ("~/work/" :depth 3))
  "Alist of directories containing repositories."
  :group 'octopus
  :type '(alist :key-type directory
                :value-type
                (plist :options
                       (((const :depth)
                         number)
                        ((const :path-fn)
                         function)))))

(defun octopus--scan-repositories ()
  (->> octopus-repo-directory-alist
    (-map (pcase-lambda (`(,root . ,plist))
            (when (file-directory-p root)
              (octopus--recurse-directories
               root (plist-get plist :depth)))))
    (-flatten-n 1)))

(cl-defstruct octopus-directory-entry path mtime)

(defun octopus--recurse-directories (root &optional depth)
  (cl-labels
      ((go (parent level)
           (if (= level 0)
               (let ((attrs (file-attributes parent)))
                 (list (make-octopus-directory-entry
                        :path (abbreviate-file-name (file-name-as-directory parent))
                        :mtime (file-attribute-modification-time attrs))))
             (->> (directory-files-and-attributes
                   parent 'full
                   (rx bol (not (any ".")))
                   nil 'nosort)
               (-map (pcase-lambda (`(,path ,dir-or-symlink . ,_))
                       (when dir-or-symlink
                         path)))
               (-non-nil)
               (--map (go it (1- level)))))))
    (-flatten (go root (or depth 0)))))

(provide 'octopus-repos)
;;; octopus-repos.el ends here
