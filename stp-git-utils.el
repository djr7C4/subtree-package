;;; -*- lexical-binding: t; -*-

(require 'f)
(require 'rem)
(require 's)
(require 'stp-bootstrap)
(require 'stp-utils)

(defun stp-git-root (&optional path)
  "Return the absolute path to the root of the git directory that path is in."
  (setq path (or path default-directory))
  (rem-with-directory path
    (db (exit-code root)
        (rem-call-process-shell-command "git rev-parse --show-toplevel")
      (setq root (s-trim root))
      (and (= exit-code 0)
           (> (length root) 0)
           (f-dir-p root)
           (f-canonical root)))))

(defmacro stp-with-git-root (&rest body)
  "Executes body in the git root for `stp-source-directory'."
  (declare (indent 0))
  `(rem-with-directory (stp-git-root stp-source-directory)
     ,@body))

(def-edebug-spec stp-with-git-root t)

(defun stp-git-tracked-p (path)
  "Determine if a file in a git repository is tracked."
  (unless (stp-git-root)
    (error "Not in a git repository"))
  (let ((dir (f-dirname path))
        (file (f-filename path)))
    (rem-with-directory dir
      (= (call-process-shell-command (concat "git ls-files --error-unmatch \"" file "\"")) 0))))

(defun stp-git-remotes ()
  (stp-with-git-root
    (db (exit-code output)
        (rem-call-process-shell-command "git remote")
      (if (= exit-code 0)
          (s-split rem-positive-whitespace-regexp output t)
        (error "Failed to list the remotes for the git repository")))))

(defun stp-git-remote-p (remote)
  "Determine if remote is the name of a git remote."
  (or (member remote (stp-git-remotes))
      (stp-with-git-root
        (= (call-process-shell-command (format "git ls-remote %s" remote)) 0))))

(defun stp-git-valid-remote-p (remote)
  "Determine if remote is a valid git repository."
  (= (call-process-shell-command (format "git ls-remote -h '%s'" remote)) 0))

(cl-defun stp-git-valid-remote-ref-p (remote ref-or-hash &optional ask-p)
  ;; Check if ref-or-hash is a ref on remote or if it is a hash that matches a
  ;; ref on remote.
  (or (member ref-or-hash (stp-git-remote-tags remote))
      (member ref-or-hash (stp-git-remote-heads remote))
      ;; There is no way to check if hash exists on a remote (only refs) so
      ;; we ask the user.
      (and ask-p
           (yes-or-no-p (format "%s was not found in %s (this is normal for hashes). Continue?" ref-or-hash remote)))))

(cl-defun stp-git-valid-ref-p (path ref)
  "Check if REF is a valid ref for the local git repository at PATH."
  (let ((default-directory path))
    (= (call-process-shell-command (format "git rev-parse --verify '%s'" ref)) 0)))

(defun stp-git-init (path)
  "Run \"git init\" on PATH."
  (let ((default-directory path))
    (unless (= (call-process-shell-command "git init") 0)
      (error "git init failed"))))

(cl-defun stp-git-add (path &optional (relative t))
  "Run \"git add\" on path. If RELATIVE is non-nil, then path will
be calculated relative to `stp-source-directory'."
  (when relative
    (setq path (f-join stp-source-directory path)))
  (let ((dir (f-dirname path))
        (target (f-filename path)))
    (rem-with-directory dir
      (db (exit-code output)
          (rem-call-process-shell-command (format "git add '%s'" target))
        (unless (= exit-code 0)
          (error "Failed to add %s to the git repository: %s" path (s-trim output)))))))

(defun stp-git-download-as-synthetic-repo (pkg-name remote)
  "Create a new git repository for PKG-NAME by downloading REMOTE
and adding it to the repository."
  (let (success
        (dir (make-temp-file pkg-name t)))
    (unwind-protect
        (progn
          (stp-download-elisp dir pkg-name remote)
          (stp-git-init dir)
          (stp-git-add dir)
          (setq success t))
      (unless success
        (f-delete dir t)))
    dir))

(defun stp-git-commit (&optional msg)
  (when (stp-git-merge-conflict-p)
    (error "Committing is not possible due to %s."
           (if (> (length (stp-git-conflicted-files)) 1)
               "merge conflicts"
             "a merge conflict")))
  (setq msg (or msg ""))
  (if (stp-git-clean-p)
      (message "There are no changes to commit. Skipping...")
    (db (exit-code output)
        (rem-call-process-shell-command (format "git commit --allow-empty-message -am '%s'" msg))
      (unless (= exit-code 0)
        (error "Failed to commit to git repository: %s" (s-trim output))))))

(defvar stp-subtree-fetch t
  "This allows hashes to be resolved when installing or upgrading.")

(defun stp-git-fetch (remote)
  (db (exit-code output)
      (rem-call-process-shell-command (format "git fetch \"%s\"" remote))
    (unless (= exit-code 0)
      (error "git fetch failed: %s" (s-trim output)))))

(defun stp-git-maybe-fetch (remote version)
  (when (and stp-subtree-fetch
             (not (stp-git-valid-remote-ref-p remote version)))
    (stp-git-fetch remote)
    t))

(defun stp-git-push ()
  (if (stp-git-unpushed-p)
      (db (exit-code output)
          (rem-call-process-shell-command "git push")
        (unless (= exit-code 0)
          (error "Failed to push to remote: %s" (s-trim output))))
    (message "There are no commits to push. Skipping...")))

(cl-defun stp-git-commit-push (msg &optional (do-commit t) (do-push t))
  (stp-with-git-root
    (when do-commit
      (stp-git-commit msg)
      ;; Pushing does not make sense if we did not commit earlier.
      (when do-push
        (stp-git-push)))))

(cl-defun stp-git-status (&key full keep-ignored keep-untracked)
  "Return a list of the status of each file in the repository. Each
status is a list containing three or four elements. The first
element is the character status code for the index used in
version 1 of the git porcelain format (see the manual for
git-status) and the second element is the character status code
for the worktree. The third is the file name. When a file is
renamed or copied, there is also a fourth element that indicates
the new name."
  (stp-with-git-root
    (let ((cmd "git status --porcelain"))
      (db (exit-code output)
          (rem-call-process-shell-command cmd)
        (unless (= exit-code 0)
          (error "%s failed: %s" cmd (s-trim output)))
        (cl-remove-if (lambda (status)
                        (db (index-status worktree-status &rest args)
                            status
                          (and (string= index-status worktree-status)
                               (member index-status
                                       (append (and keep-ignored (list "!"))
                                               (and keep-untracked (list "?")))))))
                      (mapcar (lambda (line)
                                ;; The first two characters can be spaces which have
                                ;; a specific meaning and should not be used to
                                ;; split the strings.
                                (cl-list* (substring line 0 1)
                                          (substring line 1 2)
                                          (and full (s-split " " (substring line 2)))))
                              (s-split "\n" output t)))))))

(defun stp-git-clean-p ()
  "Determine if the git repository is clean (i.e. has no uncommitted changes)."
  (and (not (stp-git-status)) t))

(defun stp-git-unpushed-p ()
  (stp-with-git-root
    (let* ((branch (stp-git-current-branch))
           (target (stp-git-push-target branch)))
      (and branch
           (db (exit-code output)
               (rem-call-process-shell-command (format "git cherry %s %s" target branch))
             (unless (= exit-code 0)
               (error "git cherry failed: %s" (s-trim output)))
             (not (string= (s-trim output) "")))))))

(defun stp-git-conflicted-files ()
  "Return the list of files with merge conflicts."
  (->> (stp-git-status :full t)
       (-filter (lambda (status)
                  (db (index-status worktree-status &rest args)
                      status
                    ;; See the description of porcelain format version 1 in
                    ;; manual for git-status.
                    (or (string= index-status "U")
                        (string= worktree-status "U")
                        (and (string= index-status worktree-status)
                             (member index-status '("A" "D")))))))
       (mapcar #'caddr)))

(defun stp-git-merge-conflict-p ()
  "Determine if there are unmerged changes."
  (and (stp-git-conflicted-files) t))

;; Based on `magit-get-current-branch'.
(defun stp-git-current-branch ()
  (stp-with-git-root
    (db (exit-code output)
        (rem-call-process-shell-command "git symbolic-ref --short HEAD")
      (unless (= exit-code 0)
        (error "Failed to get the current branch"))
      (s-trim output))))

;; Based on `magit-get-push-remote'.
(defun stp-git-push-target (&optional branch)
  (stp-with-git-root
    (setq branch (or branch (stp-git-current-branch)))
    (let ((push-default (s-trim (cadr (rem-call-process-shell-command "git config --get remote.pushDefault")))))
      (when (string= push-default "")
        (setq push-default nil))
      (or push-default
          (let ((push-remote (cadr (rem-call-process-shell-command (format "git config --get %s" (s-join "." (list "branch" branch "pushRemote")))))))
            (setq push-remote (s-trim push-remote))
            (when (string= push-remote "")
              (setq push-remote nil))
            push-remote)))))

(defvar stp-git-ask-when-unclean-p t)

(defun stp-git-clean-or-ask-p ()
  (or (not stp-git-ask-when-unclean-p)
      (stp-git-clean-p)
      (yes-or-no-p "The Git repo is unclean. Proceed anyway?")))

(defvar stp-git-abbreviated-hash-length 7)

(defun stp-git-abbreviate-hash (hash)
  (s-left stp-git-abbreviated-hash-length hash))

(defun stp-git-subtree-hash (pkg-name)
  "Determine the hash that was last merged into the subtree at pkg-name from the
remote repository."
  (let ((pkg-path (stp-canonical-path pkg-name)))
    (unless (f-dir-p pkg-path)
      (error "Package directory %s does not exist" pkg-name))
    (db (exit-code string)
        (stp-with-git-root
          (rem-call-process-shell-command (format "git log | grep \"Squashed '\\(.*/\\)\\?%s/\\?'\" | head -n 1" (stp-name pkg-name))))
      (setq string (s-trim string))
      (and (= exit-code 0)
           (> (length string) 0)
           (save-match-data
             (string-match "\\(?:[0-9a-fA-F]+..\\| \\)\\([0-9a-fA-F]+\\)$" string)
             (match-string 1 string))))))

(defun stp-git-head ()
  (stp-git-remote-head (stp-git-root stp-source-directory)))

(defun stp-git-subtree-p (pkg-name)
  "Determine if there is a git subtree for this package."
  (stp-git-subtree-hash pkg-name))

(cl-defun stp-git-remote-hash-alist (remote &rest args &key (prefixes nil prefixes-supplied-p))
  "Return an alist that maps hashes to refs. If supplied, prefixes is a list of
allowed prefixes. Only those prefixes that match a prefix in prefixes will be
kept. By default all refs are returned."
  (unless (stp-git-valid-remote-p remote)
    (error "%s is not a valid remote" remote))
  (db (exit-code string)
      (stp-with-git-root
        (rem-call-process-shell-command (format "git ls-remote %s 2> /dev/null" remote)))
    (setq string (s-trim string))
    (and (= exit-code 0)
         ;; Handle empty repositories that do not have any tags.
         (not (string= string ""))
         (mapcar (lambda (list)
                   (db (hash ref)
                       list
                     (cons hash
                           (s-chop-prefixes prefixes
                                            ref))))
                 (-filter (lambda (list)
                            (if prefixes-supplied-p
                                (-any (lambda (prefix)
                                        (db (_hash ref)
                                            list
                                          (s-starts-with-p prefix ref)))
                                      prefixes)
                              t))
                          (mapcar (lambda (line)
                                    (s-split rem-positive-whitespace-regexp line))
                                  (s-split "\n" string)))))))

(defun stp-git-remote-hash-tag-alist (remote)
  "Return an alist that maps hashes to tags."
  (stp-git-remote-hash-alist remote :prefixes '("refs/tags/")))

(defun stp-git-remote-tags (remote)
  (mapcar 'cdr (stp-git-remote-hash-tag-alist remote)))

(defun stp-git-remote-tag-p (remote ref)
  (member ref (stp-git-remote-tags remote)))

(defun stp-git-remote-hash-head-alist (remote)
  "Return an alist that maps hashes to heads."
  (stp-git-remote-hash-alist remote :prefixes '("refs/heads/")))

(defun stp-git-remote-head (remote)
  "Return HEAD for REMOTE."
  (car (rassoc "HEAD" (stp-git-remote-hash-alist remote))))

(defun stp-git-remote-heads (remote)
  (mapcar 'cdr (stp-git-remote-hash-head-alist remote)))

(defun stp-git-remote-head-p (remote ref)
  (member ref (stp-git-remote-heads remote)))

;; Note that this will not work will minimal copies of a repositories created
;; using CLI options such as those used in `stp-git-count-remote-commits'.
(cl-defun stp-git-hashes (path ref &key max)
  "Return a list of the hashes starting with the most recent that
are reachable from REF for the local git repository at PATH. If
REF is nil then HEAD will be used. If REF is t then all hashes
will be returned. If MAX is non-nil then no more than MAX hashes
will be returned."
  (setq ref (cond
             ((null ref)
              "HEAD")
             ((eq ref t)
              "--all")
             (t
              ref)))
  (let ((default-directory path))
    (db (exit-code output)
        (rem-call-process-shell-command (format "git reflog show %s%s --pretty='%%H'"
                                                ref
                                                (if max (format " -n %d" max) "")))
      (if (= exit-code 0)
          (s-split "\n" output t)
        (error "Failed to get the hashes that are reachable from %s at %s" ref path)))))

(defun stp-git-ref-to-hash (remote ref-or-hash)
  "Convert REF-OR-HASH to a hash if it isn't one already. Refs that
do not match any hash will remain unchanged."
  (or (car (or (rassoc ref-or-hash (stp-git-remote-hash-head-alist remote))
               (rassoc ref-or-hash (stp-git-remote-hash-tag-alist remote))))
      ref-or-hash))

(defun stp-git-head-to-hash (remote head-or-hash)
  "Convert HEAD-OR-HASH to a hash if it isn't one already."
  (or (car (rassoc head-or-hash (stp-git-remote-hash-head-alist remote)))
      head-or-hash))

(defun stp-git-tag-to-hash (remote tag-or-hash)
  "Convert TAG-OR-HASH to a hash if it isn't one already."
  (or (car (rassoc tag-or-hash (stp-git-remote-hash-tag-alist remote)))
      tag-or-hash))

(defun stp-git-hash= (hash hash2)
  (and (>= (length hash) 6)
       (>= (length hash2) 6)
       (or (s-prefix-p hash hash2)
           (s-prefix-p hash2 hash))))

(cl-defun stp-git-count-commits (path ref ref2 &key both)
  "Count the number of commits that have been made after REF but
before REF2 for the local git repository at PATH. If BOTH is
non-nil and the number of commits in REF..REF2 is zero, find the
number of commits n in REF2..REF and return -n."
  (unless (stp-git-valid-ref-p path ref)
    (error "%s is not a valid ref for %s" ref path))
  (unless (stp-git-valid-ref-p path ref2)
    (error "%s is not a valid ref for %s" ref2 path))
  (cl-flet ((stp-git-count-commits-forward (path ref ref2)
              (db (exit-code output)
                  (let ((default-directory path))
                    (rem-call-process-shell-command (format "git rev-list --count %s..%s" ref ref2)))
                (if (= exit-code 0)
                    (string-to-number (s-trim output))
                  (error "Failed to count the commits between %s and %s: %s" ref ref2 (s-trim output))))))
    (or (--first (/= it 0)
                 (append (list (stp-git-count-commits-forward path ref ref2))
                         (and both (list (- (stp-git-count-commits-forward path ref2 ref))))))
        0)))

(cl-defun stp-git-count-remote-commits (remote ref ref2 &key branch both)
  "This is similar to `stp-git-count-commits' except that it counts
the number of commits in the remote git repository REMOTE.
Additionally, If BRANCH is non-nil, only refs from that branch
will be considered which may improve efficiency."
  (let ((path (make-temp-file "repo-clone" t)))
    (unwind-protect
        (progn
          (db (exit-code output)
              ;; Make the call to git clone as lightweight as possible.
              (rem-call-process-shell-command (format "git clone --bare --no-checkout --filter=blob:none%s '%s' '%s'"
                                                      (if branch
                                                          (format " --single-branch --branch '%s'" branch)
                                                        "")
                                                      remote
                                                      path))
            (unless (= exit-code 0)
              (error "Failed to clone %s: %s" remote (s-trim output))))
          (stp-git-count-commits path ref ref2 :both both))
      (delete-directory path t))))

(provide 'stp-git-utils)
;;; stp-git-utils.el ends here
