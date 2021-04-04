;;; -*- lexical-binding: t -*-

(require 'buttercup)
(require 'helm-octopus)

(setq octopus-org-files (lambda () (list (expand-file-name "testdata/sample.org"))))

(describe "helm-octopus-project"
  (it "candidates"
    (expect (helm-octopus--project-group-candidates '(any-project))
            :to-be-truthy))
  (it "persistent action"
    (expect (helm-octopus--project-persistent-action)
            :to-be-truthy))
  (it "action"
    (expect (helm-octopus--project-action)
            :to-be-truthy)))

(provide 'helm-octopus-test)
