;;; -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025, David J. Rosenbaum <djr7c4@gmail.com>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of version 3 of the GNU General Public License, as
;; published by the Free Software Foundation.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require 'cl-lib)
(require 's)
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
  (or (member ref-or-hash (stp-git-remote-tags-sorted remote))
      (member ref-or-hash (stp-git-remote-heads remote))
      ;; There is no way to check if hash exists on a remote (only refs) so
      ;; we ask the user.
      (and ask-p
           (yes-or-no-p (format "%s was not found in %s (this is normal for hashes). Continue?" ref-or-hash remote)))))

(cl-defun stp-git-valid-ref-p (path ref)
  "Check if REF is a valid ref for the local git repository at PATH."
  (let ((default-directory path))
    (= (call-process-shell-command (format "git rev-parse --verify '%s'" ref)) 0)))

(cl-defun stp-git-add (path &optional (relative t))
  "Run \"git add\" on path. If relative is non-nil, then path will be
  calculated relative to `stp-source-directory'."
  (when relative
    (setq path (f-join stp-source-directory path)))
  (let ((dir (f-dirname path))
        (target (f-filename path)))
    (rem-with-directory dir
      (db (exit-code output)
          (rem-call-process-shell-command (format "git add '%s'" target))
        (unless (= exit-code 0)
          (error "Failed to add %s to the git repository: %s" path (s-trim output)))))))

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
                          (s-split "\n" (cadr (rem-call-process-shell-command "git status --porcelain")) t)))))

(defun stp-git-clean-p ()
  "Determine if the git repository is clean (i.e. has no uncommitted changes)."
  (and (not (stp-git-status)) t))

(defun stp-git-unpushed-p ()
  (stp-with-git-root
    (let* ((branch (stp-git-current-branch))
           (target (stp-git-push-target branch)))
      (and branch
           (not (string= (s-trim (cadr (rem-call-process-shell-command (format "git cherry %s %s" target branch)))) ""))))))

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
    (s-trim (cadr (rem-call-process-shell-command "git symbolic-ref --short HEAD")))))

;; Based on `magit-get-push-remote'.
(defun stp-git-push-target (&optional branch)
  "Determine if the git repository has commits that have not yet been pushed."
  (stp-with-git-root
    (setq branch (or branch (stp-git-current-branch)))
    (let ((push-default (s-trim (cadr (rem-call-process-shell-command "remote.pushDefault")))))
      (if (string= push-default "")
          (s-trim (cadr (rem-call-process-shell-command (s-join "." (list "branch" branch "pushRemote")))))
        push-default))))

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

(defun stp-normalize-version (pkg-name remote version)
  ;; If version is a hash, it might be shortened if the user entered it
  ;; manually. In this case, we replace it with the full hash from the installed
  ;; subtree.
  (if (stp-git-valid-remote-ref-p remote version)
      version
    (stp-git-subtree-hash pkg-name)))

(defun stp-git-subtree-version (pkg-info pkg-name)
  "Determine the version and the update type of the package that was
installed at the subtree. Use a tag if one is available;
otherwise, use the hash. This only works for packages that use
the \\='git method."
  (let* ((pkg-name (stp-name pkg-name)))
    (let-alist (stp-get-alist pkg-info pkg-name)
      (if (stp-git-remote-p .remote)
          (let ((cur-hash (stp-git-subtree-hash pkg-name))
                (hash-tags (stp-git-remote-hash-tag-alist .remote)))
            (if cur-hash
                (aif (cl-assoc-if (lambda (hash)
                                    (stp-git-hash= cur-hash hash))
                                  hash-tags)
                    (list (cdr it) 'stable)
                  (list cur-hash 'unstable))
              ;; When cur-hash is nil, it means that the subtree version was not
              ;; found.
              (list nil nil)))
        (list nil nil)))))

(defvar stp-git-remote-history nil)

(defun stp-git-read-remote (prompt &optional default)
  (stp-read-remote-with-predicate prompt #'stp-git-valid-remote-p default 'stp-git-remote-history))

(defvar stp-git-version-hash-separator "  ")

(defun stp-git-versions-with-hashes (remote versions)
  (let ((n (if versions
               (apply #'max (mapcar #'length versions))
             0)))
    (mapcar (lambda (version)
              (concat (string-pad version n)
                      stp-git-version-hash-separator
                      (stp-git-abbreviate-hash (stp-git-ref-to-hash remote version))))
            versions)))

(defvar stp-git-warn-unknown-version nil)

(defvar stp-git-version-history nil)

(cl-defun stp-git-read-version (prompt remote &key (extra-versions-position 'first) extra-versions default (branch-to-hash t))
  "Read a branch, tag or a hash for REMOTE. Completion is not performed on
hashes but they can be entered. EXTRA-VERSIONS is a list that is added to the
options for the version that are presented to the user. If nil appears anywhere
in extra-versions, it will be ignored. If BRANCH-TO-HASH is non-nil, branches
are converted to hashes before they are returned."
  ;; We don't complete on heads here because they are not valid versions
  ;; (hashes or tags are).
  (setq extra-versions (-filter #'identity extra-versions))
  (let (version
        (versions (->> (append (when (eq extra-versions-position 'first)
                                 extra-versions)
                               (stp-git-remote-tags-sorted remote)
                               (when (eq extra-versions-position 'last)
                                 extra-versions))
                       (stp-git-versions-with-hashes remote))))
    (while (or (not version) (not (stp-git-valid-remote-ref-p remote version stp-git-warn-unknown-version)))
      (setq version (->> (rem-comp-read prompt
                                        versions
                                        :default default
                                        :history 'stp-git-version-history
                                        :sort-fun #'identity)
                         (s-split " ")
                         car)))
    ;; Convert version to a hash if it is a branch.
    (if branch-to-hash
        (stp-git-head-to-hash remote version)
      version)))

(defun stp-git-read-update (prompt &optional default)
  "Read the update attribute."
  (intern (rem-comp-read prompt
                         ;; Some completion frameworks (e.g. vertico) don't
                         ;; handle symbols as expected when a default is
                         ;; specified.
                         '("stable" "unstable")
                         :require-match t
                         :default default
                         :sort-fun #'identity)))

(defvar stp-branch-history nil)

(defun stp-git-read-branch (prompt remote &optional default)
  "Read a branch for pkg-name."
  (let ((versions (->> (stp-git-remote-heads-sorted remote)
                       (stp-git-versions-with-hashes remote))))
    (->> (rem-comp-read prompt
                        versions
                        :require-match t
                        :default default
                        :history 'stp-branch-history
                        :sort-fun #'identity)
         (s-split " ")
         car)))

(defvar stp-git-head-explicit-order '("dev" "devel" "develop" "development" "main" "master" "stable"))

(defun stp-git-remote-heads-sorted (remote)
  (-filter (lambda (v)
             (> (length v) 0))
           (-sort (lambda (v1 v2)
                    (let ((order stp-git-head-explicit-order))
                      ;; Treat elements of
                      ;; stp-git-head-explicit-order as newer
                      ;; than anything else.
                      (cond
                       ((and (member v1 order)
                             (member v2 order))
                        (< (cl-position v1 order :test 'string=)
                           (cl-position v2 order :test 'string=)))
                       ((member v1 order)
                        t)
                       ((member v2 order)
                        nil)
                       (t
                        (string< v1 v2)))))
                  ;; Ignore branches that look like normal version strings.
                  (-filter (lambda (v)
                             (not (string-match-p stp-version-regexp v)))
                           (stp-git-remote-heads remote)))))

(defun stp-git-remote-hash-tag-alist-sorted (remote)
  (-filter (lambda (cell)
             (> (length (cdr cell)) 0))
           (reverse (-sort (lambda (cell cell2)
                             (stp-version< (cdr cell) (cdr cell2)))
                           (-filter (-compose #'stp-version-extract #'cdr)
                                    (stp-git-remote-hash-tag-alist remote))))))

(defun stp-git-remote-tags-sorted (remote)
  (mapcar #'cdr (stp-git-remote-hash-tag-alist-sorted remote)))

(defun stp-git-remote-latest-tag (remote)
  (car (stp-git-remote-tags-sorted remote)))

(cl-defun stp-git-count-commits (path ref ref2 &key both)
  "Count the number of commits that have been made after REF but
before REF2 for the local git repository at PATH. If BOTH is
non-nil and the number of commits in REF..REF2 is zero, find the
number of commits n in REF2..REF and return -n."
  (unless (stp-git-valid-ref-p path ref)
    (error "%s is not a valid ref for %s" ref remote))
  (unless (stp-git-valid-ref-p path ref2)
    (error "%s is not a valid ref for %s" ref2 remote))
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
              (error "Failed to clone %s: %s" remote output)))
          (stp-git-count-commits path ref ref2 :both both))
      (delete-directory path t))))

(defun stp-git-latest-stable-version (remote)
  (stp-git-remote-latest-tag remote))

(defun stp-git-latest-unstable-version (remote ref)
  (or (car (rassoc ref (stp-git-remote-hash-head-alist remote)))
      (and (string= ref "HEAD")
           (stp-git-remote-head remote))))

(defun stp-git-version-upgradable-p (count-to-stable count-to-unstable update)
  (if (eq update 'stable)
      (and count-to-stable (> count-to-stable 0) t)
    (and count-to-unstable (> count-to-unstable 0) t)))

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

(defun stp-git-push ()
  (if (stp-git-unpushed-p)
      (db (exit-code output)
          (rem-call-process-shell-command "git push")
        (unless (= exit-code 0)
          (error "Failed to push to remote: %s" (s-trim output))))
    (message "There are no commits to push. Skipping...")))

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

(cl-defun stp-git-install (pkg-info pkg-name remote version update &key branch (squash t))
  "Install the specified version of pkg-name from remote in
`stp-source-directory'."
  (let* ((git-root (stp-git-root stp-source-directory))
         (pkg-path (stp-canonical-path pkg-name))
         (prefix (f-relative pkg-path
                             git-root)))
    (when (f-exists-p pkg-path)
      (error "%s already exists" pkg-name))
    ;; Clone the remote repository as a squashed subtree.
    (rem-with-directory git-root
      ;; Install the package.
      (let ((hash-p (stp-git-maybe-fetch remote version)))
        (db (exit-code output)
            (rem-call-process-shell-command
             (apply #'format
                    (concat "git subtree add --prefix \"%s\" "
                            ;; When the version is a hash, don't provide a
                            ;; remote to git subtree add. This forces git to
                            ;; look for the commit locally instead which is
                            ;; possible since we previously ran git fetch.
                            (if hash-p
                                " "
                              "\"%s\" ")
                            "\"%s\"%s")
                    (append (list prefix)
                            (unless hash-p
                              (list remote))
                            (list version
                                  (if squash
                                      " --squash"
                                    "")))))
          (if (= exit-code 0)
              ;; If installation was successful, add the information for the package
              (progn
                (setq version (stp-normalize-version pkg-name remote version)
                      pkg-info (stp-set-alist pkg-info pkg-name `((method . git)
                                                                  (remote . ,remote)
                                                                  (version . ,version)
                                                                  (update . ,update))))
                (when branch
                  (setq pkg-info (stp-set-attribute pkg-info pkg-name 'branch branch))))
            (error "Failed to install %s as a git subtree: %s" pkg-name (s-trim output)))))))
  pkg-info)

(defvar stp-subtree-pull-fallback t
  "When this is non-nil and git subtree pull fails, attempt to uninstall the
package and install the new version instead.")

(cl-defun stp-git-upgrade (pkg-info pkg-name remote version &key (squash t))
  "Upgrade pkg-name in `stp-source-directory' to the specified version
from remote."
  (let* ((git-root (stp-git-root stp-source-directory))
         (pkg-path (stp-canonical-path pkg-name))
         (prefix (f-relative pkg-path
                             git-root)))
    (unless (f-exists-p pkg-path)
      (error "%s does not exist" pkg-name))
    (rem-with-directory git-root
      ;; Upgrade package
      (let* ((hash-p (stp-git-maybe-fetch remote version))
             (action (if hash-p
                         ;; merging is done instead of pulling for
                         ;; hashes because git subtree pull does
                         ;; not work for hashes on remotes.
                         "merge"
                       "pull"))
             (version-hash (stp-git-ref-to-hash remote version)))
        (when (stp-git-hash= (stp-git-subtree-hash pkg-name) version-hash)
          (user-error "Commit %s of %s is already installed"
                      (if (stp-git-hash= version version-hash)
                          (stp-git-abbreviate-hash version-hash)
                        (format "%s (%s)" (stp-git-abbreviate-hash version-hash) version))
                      pkg-name))
        (db (exit-code output)
            (rem-call-process-shell-command
             (apply #'format
                    (concat "git subtree %s --prefix \"%s\" "
                            ;; When the version is a hash, don't provide a
                            ;; remote since git subtree merge doesn't need one.
                            (if hash-p
                                " "
                              "\"%s\" ")
                            "\"%s\"%s")
                    (append (list action
                                  prefix)
                            (unless hash-p
                              (list remote))
                            (list version
                                  (if squash
                                      " --squash"
                                    "")))))
          (cond
           ;; Check for merge conflicts. These have to be dealt with manually by
           ;; the user.
           ((stp-git-merge-conflict-p)
            (message "%s occurred. Please resolve and commit manually."
                     (if (> (length (stp-git-conflicted-files)) 1)
                         "Merge conflicts"
                       "A merge conflict")))
           ((= exit-code 0))
           ;; Sometimes git subtree merge/pull fails. This can happen if the
           ;; prefix has been changed since the subtree was created. In this
           ;; case, we attempt to uninstall the package and install the new
           ;; version instead.
           ((if stp-subtree-pull-fallback
                (and (yes-or-no-p (format "git subtree %s failed: %s. Uninstall and reinstall?" action output))
                     (or stp-auto-commit
                         (yes-or-no-p "Auto commits are disabled but an auto commit is required after uninstalling. Auto commit anyway?")))
              (message "git subtree %s failed. Attempting to uninstall and reinstall..." action)
              nil)
            ;; We do not want to commit, push or perform other actions. Those
            ;; decisions are normally made in higher-level code (i.e.
            ;; `stp-upgrade'). However, git subtree add will fail if there are
            ;; uncommitted changes so we have to auto commit here. Refreshing
            ;; the stp-list-buffer-name buffer is suppressed since that will be
            ;; done by stp-upgrade (which calls this command).
            (stp-uninstall pkg-name :do-commit t :refresh nil)
            (let ((pkg-alist (stp-get-alist pkg-info pkg-name)))
              (setf (map-elt pkg-alist 'version) version)
              (stp-install pkg-name pkg-alist :refresh nil)))
           ;; Handle git subtree merge/pull errors and when the user chose not
           ;; to proceed with uninstalling and reinstalling the package.
           (t
            (error "Uninstalling and reinstalling %s failed: %s" pkg-name (s-trim output))))
          ;; If we get this far it means that either the merge succeeded or
          ;; there was a merge conflict which will be resolved manually by the
          ;; user. Either way, we update the package database.
          (if (stp-git-remote-head-p remote version)
              ;; If we update to a head (i.e. a branch), update the branch
              ;; parameter and store the current hash as the version. Since
              ;; branches are constantly updated as more commits are pushed to
              ;; the remote, storing a branch name does not make sense.
              (setq pkg-info (stp-set-attribute pkg-info pkg-name 'version version-hash)
                    pkg-info (stp-set-attribute pkg-info pkg-name 'branch version)
                    pkg-info (stp-set-attribute pkg-info pkg-name 'update 'unstable))
            ;; For tags or hashes, use the tag or hash.
            (setq version (stp-normalize-version pkg-name remote version)
                  pkg-info (stp-set-attribute pkg-info pkg-name 'version version))
            (if (stp-git-remote-tag-p remote version)
                ;; Tags do not have a branch to update from and are considered
                ;; stable.
                (setq pkg-info (stp-delete-attribute pkg-info pkg-name 'branch)
                      pkg-info (stp-set-attribute pkg-info pkg-name 'update 'stable))
              ;; If there is a 'branch attribute when updating to a hash,
              ;; leave it as is.
              (setq pkg-info (stp-set-attribute pkg-info pkg-name 'update 'unstable)))))))
    pkg-info))

(provide 'stp-git)
;;; stp-git.el ends here
