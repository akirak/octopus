;;; octopus-utils.el --- Utilities for octopus -*- lexical-binding: t -*-

(defcustom octopus-default-remote-name "origin"
  "Name of the default Git remote."
  :type 'string)

(defun octopus--default-remote-url (&optional dir)
  ;; FIXME: Don't use magit
  (let ((default-directory (or dir default-directory)))
    (car (magit-config-get-from-cached-list
          (format "remote.%s.url" octopus-default-remote-name)))))

(defun octopus--flake-url (git-url)
  "Convert a GIT-URL to an input URL in Nix flakes."
  ;; FIXME
  git-url)

(defun octopus--abbreviate-remote-url (dir)
  (-some->> (octopus--default-remote-url dir)
    (octopus--flake-url)))

(defsubst octopus--project-root (&optional maybe-prompt)
  (project-root (project-current maybe-prompt)))

(provide 'octopus-utils)
;;; octopus-utils.el ends here
