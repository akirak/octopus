;;; octopus-helm.el --- Helm interface -*- lexical-binding: t -*-

(require 'octopus-org)

(require 'helm)

(cl-defun octopus-helm--org-marker-sync-source (name markers
                                                     &key action)
  "Build a sync Helm source for Org markers."
  (declare (indent 1))
  (helm-build-sync-source name
    :candidates
    (-map (lambda (marker)
            (cons (org-with-point-at marker
                    (funcall octopus-headline-format))
                  marker))
          markers)
    :action action))

(cl-defun octopus-helm-org-marker (prompt markers &key name)
  (helm :prompt prompt
        :sources
        (octopus-helm--org-marker-sync-source name
          markers
          :action #'identity)))

(provide 'octopus-helm)
;;; octopus-helm.el ends here
