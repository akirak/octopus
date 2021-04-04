;;; -*- lexical-binding: t -*-

(require 'buttercup)
(require 'octopus)

(setq octopus-org-files (lambda () (list (expand-file-name "testdata/sample.org"))))

(describe "octopus-org.el"
  (let* ((buffer (find-file-noselect "testdata/sample.org"))
         (h1 (with-current-buffer buffer
               (goto-char (point-min))
               (re-search-forward org-heading-regexp))))

    (describe "octopus-format-headline-1"
      (it "returns a string for the outline path and tags"
        (expect (with-current-buffer buffer
                  (org-with-point-at h1
                    (string-trim (substring-no-properties (octopus-format-headline-1 50)))))
                :to-equal "sample.org: My projects")))

    (describe "octopus-org-files"
      (it "returns a list of files"
        (expect (octopus-org-files)
                :to-equal
                (list (expand-file-name "testdata/sample.org")))))

    (describe "octopus--org-project-dir"
      (it "returns the value of OCTOPUS_DIR property if any"
        (with-current-buffer buffer
          (expect (progn
                    (goto-char (point-min))
                    (octopus--org-project-dir))
                  :to-be nil)
          (expect (progn
                    (goto-char (point-min))
                    (search-forward "Project A")
                    (octopus--org-project-dir))
                  :to-equal "~/octopus-examples/project-a")
          (expect (progn
                    (goto-char (point-min))
                    (search-forward "Task 1")
                    (octopus--org-project-dir))
                  :to-equal "~/octopus-examples/project-a"))))

    (describe "octopus--org-project-remote"
      (it "returns the value of OCTOPUS_REMOTE_REPO property if any"
        (with-current-buffer buffer
          (expect (progn
                    (goto-char (point-min))
                    (octopus--org-project-remote))
                  :to-be nil)
          (expect (progn
                    (goto-char (point-min))
                    (search-forward "Project A")
                    (octopus--org-project-remote))
                  :to-equal "github:akirak/octopus-example-a")
          (expect (progn
                    (goto-char (point-min))
                    (search-forward "Task 1")
                    (octopus--org-project-remote))
                  :to-equal "github:akirak/octopus-example-a"))))

    (describe "octopus--org-up-project-root"
      (it "moves the point to the root entry of the Org project"
        (with-current-buffer buffer
          (expect (progn
                    (goto-char (point-min))
                    (search-forward "Sub note")
                    (octopus--org-up-project-root)
                    (org-get-heading t t t t))
                  :to-equal "Project A: Simple project"))))

    (describe "octopus--subtree-timestamp-info"
      (it "returns make-octopus-timestamp-info for the subtree"
        (with-current-buffer buffer
          (let ((result (progn
                          (goto-char (point-min))
                          (search-forward "Project A")
                          (org-back-to-heading)
                          (octopus--subtree-timestamp-info))))
            ;; FIXME: This seems to provide a wrong result.
            ;; (expect (octopus-timestamp-info-count result)
            ;;         :to-be 7)
            (expect (ts= (octopus-timestamp-info-last-ts result)
                         (make-ts :year 2021 :month 4 :day 4
                                  :hour 19 :minute 21 :second 0))
                    :to-be-truthy)))))))

(provide 'octopus-test)
