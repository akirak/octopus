;;; org-project-bridge.el --- Integration between Org and project.el -*- lexical-binding: t -*-

(defun akirak/git-remote-origin-abbreviate-url (dir)
  (let ((default-directory dir))
    (-some->> (magit-config-get-from-cached-list "remote.origin.url")
      (car)
      (akirak/parse-git-url)
      (akirak/remote-git-repo-abbreviate-url))))

(defun akirak/org-ql-predicate-for-project-subtrees (root)
  (let ((remote-repo (akirak/git-remote-origin-abbreviate-url root)))
    `(and (not (tags "ARCHIVE"))
          (or (property "PROJECT_DIR" ,(f-short root))
              (property "PROJECT_REMOTE_REPO" ,remote-repo)))))

(cl-defun akirak/org-subtrees-for-project (&optional root
                                                     &key (action 'element-with-markers)
                                                     (sort
                                                      (-on #'>
                                                           (lambda (x)
                                                             (cond
                                                              ((org-element-property :PROJECT_DIR x)
                                                               2)
                                                              ((org-element-property :PROJECT_REMOTE_REPO x)
                                                               1))))))
  (let ((root (or root (project-root (project-current t)))))
    (org-ql-select (org-agenda-files)
      (akirak/org-ql-predicate-for-project-subtrees root)
      :action action
      :sort sort)))

(cl-defun akirak/helm-build-org-marker-sync-source (name markers &key (action #'identity))
  (declare (indent 1))
  (helm-build-sync-source name
    :candidates
    (-map (lambda (marker)
            (cons (org-with-point-at marker
                    (format "%s: %s %s"
                            (buffer-name)
                            (org-format-outline-path
                             (org-get-outline-path t t))
                            (org-get-tags-string)))
                  marker))
          markers)
    :action action))

(defun akirak/org-narrow-to-project-subtree ()
  (interactive)
  (let ((marker (pcase (akirak/org-subtrees-for-project)
                  (`nil (user-error "No subtree for the project"))
                  (`(,marker) marker)
                  (markers
                   (helm :prompt "Select a subtree: "
                         :sources
                         (akirak/helm-build-org-marker-sync-source "Org subtrees for the project"
                                                                   markers))))))
    (switch-to-buffer-other-window (marker-buffer marker))
    (widen)
    (goto-char marker)
    (org-narrow-to-subtree)))

(defmacro akirak/select-from-other-windows (prompt exp)
  "Evaluate EXP in each live window and select a value different from the current one."
  (declare (indent 1))
  `(let* ((current ,exp)
          (options (->> (let (results)
                          (walk-windows
                           (lambda (w)
                             (push (with-current-buffer (window-buffer w)
                                     ,exp)
                                   results)))
                          results)
                        (-non-nil)
                        (-uniq)
                        (-remove-item current))))
     (completing-read ,prompt options)))

(defun akirak/org-set-project-dir ()
  (interactive)
  (assert (and (derived-mode-p 'org-mode)
               (not (org-before-first-heading-p))))
  (when (org-entry-get nil "PROJECT_DIR" t)
    (user-error "This entry already has PROJECT_DIR property set"))
  (org-entry-put nil "PROJECT_DIR"
                 (akirak/select-from-other-windows "Project directory: "
                                                   (project-root (project-current)))))

(defun akirak/org-set-project-remote-repo (&optional arg)
  (interactive "P")
  (assert (and (derived-mode-p 'org-mode)
               (not (org-before-first-heading-p))))
  (when (org-entry-get nil "PROJECT_REMOTE_REPO" t)
    (user-error "This entry already has PROJECT_REMOTE_REPO property set"))
  (org-entry-put nil "PROJECT_REMOTE_REPO"
                 (if arg
                     (->> (magit-list-repos)
                          (-map #'akirak/git-remote-origin-abbreviate-url)
                          (-non-nil)
                          (-uniq)
                          (completing-read "Origin: "))
                   (akirak/select-from-other-windows "Origin: "
                                                     (akirak/git-remote-origin-abbreviate-url)))))

(defcustom akirak/org-ql-project-todo-groups
  (or (bound-and-true-p akirak/org-super-agenda-todo-groups)
      '((:todo "DONE" :order 5)
        (:auto-planning t)
        (:todo "STARTED")
        (:todo "WAITING")
        (:todo "REVIEW")
        (:todo "NEXT")
        (:todo "STOPPED")
        (:todo t)))
  "FIXME: Docstring")

(defun akirak/org-ql-project-todo-list (&optional arg)
  (interactive "P")
  (if (equal arg '(16))
      (akirak/org-ql-remote-project-todo-list
       (akirak/org-agenda-files-select-project-remote-repo))
    (let* ((root (if arg
                     (akirak/org-agenda-files-select-project-dir)
                   (project-root (project-current))))
           (default-directory root))
      (org-ql-search (org-agenda-files)
        `(and (not (tags "ARCHIVE"))
              (todo)
              (not (todo "DONE"))
              (ancestors ,(akirak/org-ql-predicate-for-project-subtrees root)))
        :title (format "Project %s" root)
        :super-groups akirak/org-ql-project-todo-groups))))

(defun akirak/org-ql-remote-project-todo-list (remote)
  (org-ql-search (org-agenda-files)
    `(and (not (tags "ARCHIVE"))
          (todo)
          (not (todo "DONE"))
          (ancestors (property "PROJECT_REMOTE_REPO" ,remote)))
    :title (format "Project %s" remote)
    :super-groups akirak/org-ql-project-todo-groups))

(cl-defun akirak/org-select-property-value (property &key files)
  (->> (org-ql-select (or files (org-agenda-files))
         `(and (not (tags "ARCHIVE"))
               (property ,property))
         :action `(org-entry-get nil property))
       (-uniq)
       (-sort #'string<)
       (completing-read (format "%s: " property))))

(defun akirak/org-agenda-files-select-project-dir ()
  (akirak/org-select-property-value "PROJECT_DIR"))

(defun akirak/org-agenda-files-select-project-remote-repo ()
  (akirak/org-select-property-value "PROJECT_REMOTE_REPO"))

(defun akirak/org-project-find-local-repository ()
  (interactive)
  (if-let (root (org-entry-get nil "PROJECT_DIR"))
      (magit-status root)
    (if-let (remote-repo (org-entry-get nil "PROJECT_REMOTE_REPO"))
        (pcase (->> (magit-list-repos)
                    (-filter (lambda (root)
                               (string-equal remote-repo
                                             (akirak/git-remote-origin-abbreviate-url root)))))
          (`(,x)
           (magit-status root))
          (`nil
           (user-error "No local copy of the repository %s" remote-repo))
          (xs
           (magit-status (completing-read "Local copy: " xs))))
      (user-error "No property for the project identity"))))

(defun akirak/org-project-create-subtree (&optional arg)
  (interactive "P")
  (pcase arg
    (`(16)
     (->> (org-ql-select (org-agenda-files)
            '(and (not (tags "ARCHIVE"))
                  (or (property "PROJECT_DIR")
                      (property "PROJECT_REMOTE_REPO")))
            :action 'point-marker)
          (akirak/helm-build-org-marker-sync-source
           "Subtrees with project property")
          (helm :prompt "Project subtree: "
                :sources)
          (org-goto-marker-or-bmk)))
    (_
     (let* ((contexts (org-ql-select (org-agenda-files)
                        `(children (and (not (tags "ARCHIVE"))
                                        (or (property "PROJECT_DIR")
                                            (property "PROJECT_REMOTE_REPO"))))
                        :action '(prog1 (point-marker)
                                   (org-end-of-subtree))))
            (parent-marker (helm :prompt "Project context: "
                                 :sources
                                 (akirak/helm-build-org-marker-sync-source
                                  "Parents of existing project subtrees"
                                  contexts)))
            (org-capture-entry `("p" "New project"
                                 entry
                                 (function (lambda ()
                                             (org-goto-marker-or-bmk ,parent-marker)))
                                 "* %?"
                                 :clock-in t :clock-resume t)))
       (org-capture)))))

(cl-defun akirak/org-capture-find-project-todo-destination (&key root remote)
  (let* ((root (unless remote
                 (or root
                     (project-root (project-current)))))
         (identity (or remote (f-short root)))
         (files (org-agenda-files))
         (subtree-pred (if remote
                           `(property "PROJECT_REMOTE_REPO" ,remote)
                         (akirak/org-ql-predicate-for-project-subtrees root)))
         (parents (or (org-ql-select files
                        `(and (ancestors ,subtree-pred)
                              (not (tags "ARCHIVE"))
                              (property "PROJECT_CAPTURE_LOCATION"))
                        :action #'point-marker)
                      (org-ql-select files
                        `(and (ancestors ,subtree-pred)
                              (not (tags "ARCHIVE"))
                              (children (todo)))
                        :action `(prog1 (point-marker)
                                   (org-end-of-subtree)))
                      (org-ql-select files
                        `(and ,subtree-pred
                              (not (tags "ARCHIVE")))
                        :action #'point-marker)))
         (parent (pcase parents
                   (`nil (user-error "Cannot find project destination"))
                   (`(,marker) marker)
                   (_ (helm :prompt "Select a capture location: "
                            :sources
                            (akirak/helm-build-org-marker-sync-source
                             (format "Project %s" identity)
                             parents))))))
    (switch-to-buffer (marker-buffer parent))
    (widen)
    (goto-char parent)))

(cl-defun akirak/org-capture-project-todo (&key (todo-state "TODO")
                                                target)
  (interactive (list :target
                     (pcase current-prefix-arg
                       (`(4)
                        (let ((dir (akirak/org-agenda-files-select-project-dir)))
                          `(lambda ()
                             (akirak/org-capture-find-project-todo-destination
                              :root ,dir))))
                       (`(16)
                        (let ((remote (akirak/org-agenda-files-select-project-remote-repo)))
                          `(lambda ()
                             (akirak/org-capture-find-project-todo-destination
                              :remote ,remote)))))))
  (let* ((location-fn (or target
                          #'akirak/org-capture-find-project-todo-destination))
         (org-capture-entry `("p" "Project todo"
                              entry
                              (function ,location-fn)
                              ,(format "* %s %%? %%^g
:PROPERTIES:
:CREATED_TIME: %%U
:END:
" todo-state)
                              :clock-in t :clock-resume t)))
    (org-capture)))

(defun akirak/org-capture-start-project-todo (title)
  (interactive "sName of the task: ")
  (let ((org-capture-initial title)
        (org-capture-entry `("p" "Project todo"
                             entry
                             (function akirak/org-capture-find-project-todo-destination)
                             "* STARTED %i
:PROPERTIES:
:CREATED_TIME: %%U
:END:

%a

%?
"                             
                             :clock-in t :clock-resume t)))
    (org-capture)))

(provide 'org-project-bridge)
;;; org-project-bridge.el ends here
