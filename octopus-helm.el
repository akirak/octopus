;;; octopus-helm.el --- Helm interface for octopus -*- lexical-binding: t -*-

(require 'octopus-org)

(require 'helm)

(cl-defun octopus-helm--org-marker-sync-source (name markers
                                                     &key action)
  "Build a sync Helm source for Org markers.

NAME will be the name of the Helm source.

It creates an interface that lets the user select an item from MARKERS.

`octopus-headline-format' is used to format each candidate.

You must specify the ACTION of the Helm source."
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
  "Let the user select an Org marker via Helm.

PROMPT is the prompt of the minibuffer.

It lets the user select one of MARKERS.

NAME will be the name of the Helm sync source."
  (helm :prompt prompt
        :sources
        (octopus-helm--org-marker-sync-source name
          markers
          :action #'identity)))

(provide 'octopus-helm)
;;; octopus-helm.el ends here
