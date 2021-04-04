;;; -*- lexical-binding: t -*-

(require 'buttercup)
(require 'octopus-utils)

(describe "octopus--default-remote-url"
  (it "Get the remote URL")
  (it "Doesn't alter other URLs"
    (expect (octopus--flake-url "https://gitlab.com/hello/bbbb.git")
            :to-equal "https://gitlab.com/hello/bbbb.git")
    (expect (octopus--flake-url "git+https://git.somehost.tld/user/path")
            :to-equal "git+https://git.somehost.tld/user/path")))

(describe "octopus--flake-url"
  (it "Converts a GitHub URL"
    (expect (octopus--flake-url "https://github.com/foo/bar.cpp")
            :to-equal "github:foo/bar.cpp")
    (expect (octopus--flake-url "https://github.com/foo12345/bar-12345/")
            :to-equal "github:foo12345/bar-12345")
    (expect (octopus--flake-url "https://github.com/foo/bar.git")
            :to-equal "github:foo/bar")
    (expect (octopus--flake-url "https://github.com/foo/bar.git/")
            :to-equal "github:foo/bar")
    (expect (octopus--flake-url "git@github.com:foo/bar")
            :to-equal "github:foo/bar")
    (expect (octopus--flake-url "git@github.com:foo/bar.git")
            :to-equal "github:foo/bar"))
  (it "Converts a GitHub URL at a branch"
    (expect (octopus--flake-url "https://github.com/akirak/example/blob/dev-hello/")
            :to-equal "github:akirak/example/dev-hello")
    (expect (octopus--flake-url "https://github.com/akirak/example/blob/dev-hello/hello.el")
            :to-equal "github:akirak/example/dev-hello")
    (expect (octopus--flake-url "https://github.com/akirak/example/blob/fix/issue-12345/abcd.el")
            :to-equal "github:akirak/example/fix"))

  (it "Doesn't alter other URLs"
    (expect (octopus--flake-url "https://gitlab.com/hello/bbbb.git")
            :to-equal "https://gitlab.com/hello/bbbb.git")
    (expect (octopus--flake-url "git+https://git.somehost.tld/user/path")
            :to-equal "git+https://git.somehost.tld/user/path")))

(describe "octopus--format-time"
  (it "Prints the date if it is more than 48 hours ago"
    (expect (octopus--format-time (ts-unix (make-ts :year 2020 :month 1 :day 1
                                                    :hour 0 :minute 0 :second 0)))
            :to-equal "2020-01-01 (Wed)")
    (expect (octopus--format-time (ts-unix (make-ts :year 2020 :month 1 :day 1
                                                    :hour 0 :minute 0 :second 0))
                                  (ts-unix (make-ts :year 2020 :month 1 :day 3
                                                    :hour 1 :minute 0 :second 0)))
            :to-equal "2020-01-01 (Wed)"))
  (it "Prints the duration if it is less than 48 hours ago"
    (expect (octopus--format-time (ts-unix (make-ts :year 2020 :month 1 :day 1
                                                    :hour 0 :minute 0 :second 0))
                                  (ts-unix (make-ts :year 2020 :month 1 :day 2
                                                    :hour 23 :minute 0 :second 0)))
            :to-equal "2 days ago")))

(describe "octopus--format-duration"
  (it "just now"
    (expect (octopus--format-duration (ts-difference (make-ts :year 2020 :month 1 :day 1
                                                              :hour 0 :minute 0 :second 2)
                                                     (make-ts :year 2020 :month 1 :day 1
                                                              :hour 0 :minute 0 :second 0)))
            :to-equal "just now"))
  (it "minutes"
    (expect (octopus--format-duration (ts-difference (make-ts :year 2020 :month 1 :day 1
                                                              :hour 0 :minute 2 :second 3)
                                                     (make-ts :year 2020 :month 1 :day 1
                                                              :hour 0 :minute 0 :second 0)))
            :to-equal "2 minutes ago"))
  (it "hours"
    (expect (octopus--format-duration (ts-difference (make-ts :year 2020 :month 1 :day 1
                                                              :hour 1 :minute 2 :second 3)
                                                     (make-ts :year 2020 :month 1 :day 1
                                                              :hour 0 :minute 0 :second 0)))
            :to-equal "1 hours ago"))
  (it "days"
    (expect (octopus--format-duration (ts-difference (make-ts :year 2020 :month 1 :day 2
                                                              :hour 0 :minute 2 :second 1)
                                                     (make-ts :year 2020 :month 1 :day 1
                                                              :hour 0 :minute 0 :second 0)))
            :to-equal "1 days ago"))
  (it "months"
    (expect (octopus--format-duration (ts-difference (make-ts :year 2020 :month 5 :day 20
                                                              :hour 0 :minute 2 :second 3)
                                                     (make-ts :year 2020 :month 1 :day 1
                                                              :hour 0 :minute 0 :second 0)))
            :to-equal "5 months ago"))
  (it "year"
    (expect (octopus--format-duration (ts-difference (make-ts :year 2020 :month 5 :day 9
                                                              :hour 0 :minute 2 :second 3)
                                                     (make-ts :year 2019 :month 1 :day 1
                                                              :hour 0 :minute 0 :second 0)))
            :to-equal "1 years ago")))

(describe "octopus-merge-timestamp-info"
  (it "Merge multiple timestamps"
    (let ((ts-a (make-ts :year 2020 :month 1 :day 1
                         :hour 0 :minute 0 :second 0))
          (ts-b (make-ts :year 2021 :month 1 :day 1
                         :hour 0 :minute 0 :second 0))
          (ts-c (make-ts :year 2022 :month 1 :day 1
                         :hour 0 :minute 0 :second 0)))
      (let ((merged-ts (octopus-merge-timestamp-info
                        (make-octopus-timestamp-info :last-ts ts-a :count 1)
                        (make-octopus-timestamp-info :last-ts ts-b :count 2))))
        (expect (ts= (octopus-timestamp-info-last-ts merged-ts) ts-b) :to-be-truthy)
        (expect (octopus-timestamp-info-count merged-ts) :to-be 3))
      (let ((merged-ts (octopus-merge-timestamp-info
                        (make-octopus-timestamp-info :last-ts ts-c :count 2)
                        (make-octopus-timestamp-info :last-ts ts-a :count 5))))
        (expect (ts= (octopus-timestamp-info-last-ts merged-ts) ts-c) :to-be-truthy)
        (expect (octopus-timestamp-info-count merged-ts) :to-be 7))
      (let ((merged-ts (octopus-merge-timestamp-info
                        (make-octopus-timestamp-info :last-ts nil :count 0)
                        (make-octopus-timestamp-info :last-ts ts-a :count 5))))
        (expect (ts= (octopus-timestamp-info-last-ts merged-ts) ts-a) :to-be-truthy)
        (expect (octopus-timestamp-info-count merged-ts) :to-be 5)))))

(describe "octopus-timestamp-info-frecency"
  (it "Calculate the frecency"
    (expect (octopus-timestamp-info-frecency
             (make-octopus-timestamp-info :last-ts (make-ts :year 2020 :month 1 :day 1
                                                            :hour 0 :minute 0 :second 0)
                                          :count 5))
            :to-be-truthy)
    (expect (octopus-timestamp-info-frecency
             (make-octopus-timestamp-info :last-ts nil
                                          :count 0))
            :to-be 0)))

(provide 'octopus-utils-test)
