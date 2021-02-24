;;; octopus-org.el --- Org-related definitions and utilities for octopus -*- lexical-binding: t -*-

(defconst octopus-dir-property-name "PROJECT_DIR"
  "Org property that locates the root directory of a project.")

(defconst octopus-remote-repo-property-name "PROJECT_REMOTE_REPO"
  "Org property that locates the remote repository of a project.")

(defcustom octopus-org-files #'org-agenda-files
  "Function to retrieve a list of source Org files."
  :type 'function)

(defcustom octopus-display-org-buffer-function
  #'pop-to-buffer
  ;; #'switch-to-buffer-other-window
  "Function used to display an Org buffer."
  :type 'function)

(defcustom octopus-org-tree-show-function
  #'org-narrow-to-subtree
  "Function used to show the subtree at point."
  :type 'function)

(defcustom octopus-headline-format
  #'octopus-format-headline-1
  "Function used to format a headline for display.

This is used to build each candidate in `completing-read'-like
interfaces in the package.

The function is called with no arguments at the marker position."
  :type 'function)

(defun octopus-format-headline-1 ()
  "Default implementation of `octopus-headline-format'."
  (format "%s: %s %s"
          (buffer-name)
          (org-format-outline-path
           (org-get-outline-path t t))
          (org-get-tags-string)))

(defun octopus--display-org-marker (marker)
  "Display an Org MARKER using designated functions.

This uses `octopus-display-org-buffer-function' and
`octopus-org-tree-show-function'."
  (with-current-buffer (marker-buffer marker)
    (funcall octopus-display-org-buffer-function (current-buffer))
    (widen)
    (goto-char marker)
    (when (boundp octopus-org-tree-show-function)
      (funcall octopus-org-tree-show-function))))

(defsubst octopus-org-files ()
  "Return the Org files of interest.

This just calls `octopus-org-files'."
  (funcall octopus-org-files))

(defsubst octopus--org-project-dir ()
  "Return the root directory of a project set in the property, if any"
  (org-entry-get nil octopus-dir-property-name t))

(defsubst octopus--org-project-remote ()
  "Return the remote repository of a project set in the property, if any"
  (org-entry-get nil octopus-remote-repo-property-name t))

(provide 'octopus-org)
;;; octopus-org.el ends here
