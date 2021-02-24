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

(provide 'octopus-utils-test)
