;;; octopus-org.el --- Org-related definitions for octopus -*- lexical-binding: t -*-

(defconst octopus-dir-property-name "PROJECT_DIR")
(defconst octopus-remote-repo-property-name "PROJECT_REMOTE_REPO")

(defcustom octopus-org-files #'org-agenda-files
  "Function to retrieve a list of source Org files."
  :type 'function)

(defsubst octopus-org-files ()
  (funcall octopus-org-files))

(defcustom octopus-headline-format
  #'octopus-format-headline-1
  "FIXME

The function is called with no arguments at the marker position."
  :type 'function)

(defun octopus-format-headline-1 ()
  (format "%s: %s %s"
          (buffer-name)
          (org-format-outline-path
           (org-get-outline-path t t))
          (org-get-tags-string)))

(defmacro octopus--org-put-new-property-with (property exp)
  (declare (indent 1))
  `(progn
     (assert (and (derived-mode-p 'org-mode)
                  (not (org-before-first-heading-p))))
     (when (org-entry-get nil ,property t)
       (user-error "This entry already has %s property set" ,property))
     (org-entry-put nil ,property ,exp)))

(defsubst octopus--org-project-dir ()
  (org-entry-get nil octopus-dir-property-name t))

(defsubst octopus--org-project-remote ()
  (org-entry-get nil octopus-remote-repo-property-name t))

(provide 'octopus-org)
;;; octopus-org.el ends here
