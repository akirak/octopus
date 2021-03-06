;;; octopus-board.el --- synopsis -*- lexical-binding: t -*-

(defsubst octopus--has-git-p (dir)
  (file-exists-p (f-join dir ".git")))

(defcustom octopus-repository-enumerate-function
  (defun octopus-scan-magit-directories ()
    (cl-labels
        ((go (depth dir)
             (when (and (file-directory-p dir)
                        (not (string-match-p (rx "/.") dir)))
               (if (= depth 0)
                   (list (file-name-as-directory dir))
                 (cl-mapcan (-partial #'go (1- depth))
                            (directory-files dir t))))))
      (->> magit-repository-directories
           (-map (pcase-lambda (`(,dir . ,depth))
                   (go depth dir)))
           (-flatten-n 1))))
  "FIXME"
  :type 'function)

(defcustom octopus-non-git-dir-predicate
  (defun octopus--meaningful-non-git-dir-p (dir)
    ;; FIXME
    (string-match-p (rx bol (literal (expand-file-name "~/"))
                        (or "work" "projects")
                        "/"
                        anything)
                    dir))
  "FIXME"
  :type 'function)

(defun octopus-scan ()
  (interactive)
  (->> (funcall octopus-repository-enumerate-function)
       (-map #'f-short)
       (--filter (or (octopus--has-git-p it)
                     (funcall octopus-non-git-dir-predicate it)))
       (octopus--uniq-files)))

;;;; Commands

(defconst octopus-board-buffer "*octopus board*")

(defun octopus-board ()
  (interactive)
  (let* ((known-projects (->> (octopus--ql-select '(any-project)
                                :action
                                '(let ((project (list :olp (org-get-outline-path t t)
                                                      :buffer (buffer-name)
                                                      :marker (point-marker)))
                                       (local-dir (org-entry-get nil octopus-dir-property-name))
                                       (remote-repo (org-entry-get nil octopus-remote-repo-property-name)))
                                   (-non-nil
                                    (list
                                     (when local-dir
                                       (cons 'local-dir (cons local-dir project)))
                                     (when remote-repo
                                       (cons 'remote-repo (cons remote-repo project)))))))
                              (-flatten-n 1)
                              (-group-by #'car)
                              (--map (cons (car it) (-map #'cdr (cdr it))))))
         (local-dir-map (alist-get 'local-dir known-projects))
         (remote-repo-map (alist-get 'remote-repo known-projects))
         (status (-map (lambda (dir)
                         (cons dir
                               (or (assoc dir local-dir-map)
                                   (when-let (remote (octopus--abbreviate-remote-url dir))
                                     (assoc remote remote-repo-map)))))
                       (octopus-scan)))
         (registered (-map #'car (-filter #'cdr status)))
         (unregistered (-map #'car (-filter (-compose #'not #'cdr) status))))
    (with-current-buffer (get-buffer-create octopus-board-buffer)
      (magit-section-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (when registered
          (magit-insert-section (registered)
            (magit-insert-heading "Registered repositories")
            (insert (string-join registered "\n"))))
        (when unregistered
          (magit-insert-section (unregistered)
            (magit-insert-heading "Unregistered repositories")
            (insert (string-join unregistered "\n")))))
      ;; "Content outside of the section"
      (pop-to-buffer (current-buffer)))))

(provide 'octopus-board)
;;; octopus-board.el ends here
