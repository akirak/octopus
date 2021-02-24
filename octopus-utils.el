;;; octopus-utils.el --- Utilities for octopus -*- lexical-binding: t -*-

(defcustom octopus-default-git-remote-name "origin"
  "Name of the default Git remote."
  :type 'string)

(defun octopus--default-git-remote-url (&optional dir)
  "Return the URL of the default Git remote at DIR.

The remote name is specified by `octopus-default-git-remote-name'."
  (let ((default-directory (or dir default-directory)))
    (car (octopus--get-git-config-local
          (format "remote.%s.url" octopus-default-git-remote-name)))))

(defun octopus--get-git-config-local (key)
  "Return the value of KEY from the local Git config."
  (-> (octopus--read-process "git" "config" "--local" "--get" key)
      (split-string "\n")
      (-non-nil)))

(defun octopus--read-process (program &rest args)
  "Return the standard output from PROGRAM run with ARGS."
  (with-temp-buffer
    (if (zerop (apply #'call-process program
                      nil (list (current-buffer) nil) nil
                      args))
        (buffer-string)
      (error "Process %s %s returned non-zero" program args))))

;; Regular expression pattern from git-identity.el
(eval-and-compile
  (defconst octopus--git-xalpha
    ;; TODO: Add thorough tests and fix this pattern
    (let* ((safe "-$=_@.&+")
           (extra "!*(),~")
           ;; I don't think people would want URLs containing
           ;; double/single quotes, but the spec contains them.
           ;;
           ;; (extra "!*\"'(),")
           (escape '(and "%" (char hex) (char hex))))
      `(or ,escape (char alpha digit ,safe ,extra)))))

(defun octopus--flake-url (git-url)
  "Convert a GIT-URL to an input URL in Nix flakes."
  (save-match-data
    (cond
     ((string-match (rx bol "git@github.com:"
                        (group (* (and (+ (eval octopus--git-xalpha)) "/"))
                               (+? (eval octopus--git-xalpha)))
                        (?  ".git")
                        eol)
                    git-url)
      (concat "github:" (match-string 1 git-url)))
     ((string-match (rx bol "https://github.com/"
                        (group (+ (eval octopus--git-xalpha))
                               "/"
                               (+? (eval octopus--git-xalpha)))
                        (or (and (?  ".git")
                                 (?  "/")
                                 eol)
                            (and "/blob/"
                                 (group (+ (eval octopus--git-xalpha)))
                                 "/")))
                    git-url)
      (concat "github:" (match-string 1 git-url)
              (-some->> (match-string 2 git-url)
                (concat "/"))))
     (t
      git-url))))

(defun octopus--abbreviate-remote-url (dir)
  "Return the flake URL of the default remote at DIR."
  (-some->> (octopus--default-git-remote-url dir)
    (octopus--flake-url)))

(defsubst octopus--project-root (&optional maybe-prompt)
  "Return the project root at the default directory.

This is just a combination `project-root' and `project-current'
which takes MAYBE-PROMPT as an argument, which see."
  (project-root (project-current maybe-prompt)))

(provide 'octopus-utils)
;;; octopus-utils.el ends here
