;;; -*- lexical-binding: t; -*-

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

(defun stp-git-clean-p ()
  "Determine if the git repository is clean (i.e. has no uncommitted changes)."
  (stp-with-git-root
    (string= (s-trim (cadr (rem-call-process-shell-command "git status --porcelain | grep -v '^??'"))) "")))

(defvar stp-git-ask-when-unclean-p t)

(defun stp-git-clean-or-ask-p ()
  (or (not stp-git-ask-when-unclean-p)
      (stp-git-clean-p)
      (yes-or-no-p "The Git repo is unclean. Proceed anyway?")))

(defun stp-git-subtree-hash (pkg-name)
  "Determine the hash that was last merged into the subtree at pkg-name from the
remote repository."
  (let ((pkg-path (stp-absolute-path pkg-name)))
    (when (not (f-dir-p pkg-path))
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

(defun stp-git-subtree-p (pkg-name)
  "Determine if there is a git subtree for this package."
  (stp-git-subtree-hash pkg-name))

(cl-defun stp-git-remote-hash-alist (remote &key (prefixes nil prefixes-supplied-p))
  "Return an alist that maps hashes to refs. If supplied, prefixes is a list of
allowed prefixes. Only those prefixes that match a prefix in prefixes will be
kept. By default all refs are returned."
  (when (not (stp-git-remote-p remote))
    (error "%s is not a valid remote" remote))
  (db (exit-code string)
      (stp-with-git-root
        (rem-call-process-shell-command (format "git ls-remote %s 2> /dev/null" remote)))
    (setq string (s-trim string))
    (and (= exit-code 0)
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

(defun stp-git-remote-heads (remote)
  (mapcar 'cdr (stp-git-remote-hash-head-alist remote)))

(defun stp-git-remote-head-p (remote ref)
  (member ref (stp-git-remote-heads remote)))

(defun stp-git-hash= (hash hash2)
  (cl-assert (and (>= (length hash) 6)
               (>= (length hash2) 6)))
  (or (s-prefix-p hash hash2)
      (s-prefix-p hash2 hash)))

(defun stp-git-subtree-version (pkg-info pkg-name)
  "Determine the version and the update type of the package that was
installed at the subtree. Use a tag if one is available;
otherwise, use the hash. This only works for packages that use
the \\='git method."
  (let* ((pkg-name (stp-name pkg-name))
         (pkg-remote (stp-get-attribute pkg-info pkg-name 'remote)))
    (if (stp-git-remote-p pkg-remote)
        (let ((cur-hash (stp-git-subtree-hash pkg-name))
              (hash-tags (stp-git-remote-hash-tag-alist pkg-remote)))
          (if cur-hash
              (aif (cl-assoc-if (lambda (hash)
                                  (stp-git-hash= cur-hash hash))
                                hash-tags)
                  (list (cdr it) 'stable)
                (list cur-hash 'unstable))
            ;; When cur-hash is nil, it means that the subtree version was not
            ;; found.
            (list nil nil)))
      (list nil nil))))

(defvar stp-git-remote-history nil)

(defun stp-git-read-remote (prompt &optional default)
  (stp-read-remote-with-predicate prompt #'stp-git-valid-remote-p default 'stp-git-remote-history))

(defvar stp-git-version-history nil)

(cl-defun stp-git-read-version (prompt remote &key (extra-versions-position 'first) extra-versions default (branch-to-hash t))
  "Read a branch, tag or a hash for REMOTE. Completion is not performed on
hashes but they can be entered. EXTRA-VERSIONS is a list that is added to the
options for the version that are presented to the user. If nil appears anywhere
in extra-versions, it will be ignored. If BRANCH-TO-HASH is non-nil, branches
are converted to hashes before they are returned."
  ;; We don't complete on heads here because they are not valid versions
  ;; (hashes or tags are).
  (let (version
        (tags (stp-git-remote-tags-sorted remote)))
    (while (or (not version) (not (stp-git-valid-remote-ref-p remote version t)))
      (setq version (rem-comp-read prompt
                                   (append (when (eq extra-versions-position 'first)
                                             extra-versions)
                                           tags
                                           (when (eq extra-versions-position 'last)
                                             extra-versions))
                                   :default default
                                   :history 'stp-git-version-history
                                   :sort-fun #'identity)))
    ;; Convert version to a hash if it is a branch.
    (if (and branch-to-hash
             (member version (stp-git-remote-heads-sorted remote)))
        (car (rassoc version (stp-git-remote-hash-head-alist remote)))
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

(defun stp-git-read-branch (prompt remote &optional default)
  "Read a branch for pkg-name."
  (rem-comp-read prompt
                 (stp-git-remote-heads-sorted remote)
                 :require-match t
                 :default default
                 :sort-fun #'identity))

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

(defun stp-git-remote-tags-sorted (remote)
  (-filter (lambda (v)
             (> (length v) 0))
           (reverse (-sort 'stp-version<
                           (-filter 'stp-version-extract
                                    (stp-git-remote-tags remote))))))

(defun stp-git-filtered-tags (pkg-info &optional dir)
  "Find all tags that are not recognized by `stp-version-extract'."
  (when (not dir)
    (setq dir stp-source-directory))
  (-filter 'cdr
           (mapcar (lambda (pkg)
                     (cons (f-filename pkg)
                           (-filter (lambda (v)
                                      (not (stp-version-extract v)))
                                    (stp-git-remote-tags pkg))))
                   (-filter (lambda (pkg-name)
                              (stp-git-remote-p (stp-get-attribute pkg-info pkg-name 'remote)))
                            (stp-info-names)))))

(defun stp-git-commit (&optional msg)
  (setq msg (or msg ""))
  (db (exit-code output)
      (rem-call-process-shell-command (format "git commit --allow-empty-message -am '%s'" msg))
    (unless (= exit-code 0)
      (error "Failed to commit to git repository: %s" (s-trim output)))))

(defun stp-git-push ()
  (db (exit-code output)
      (rem-call-process-shell-command "git push")
    (unless (= exit-code 0)
      (error "Failed to push to remote: %s" (s-trim output)))))

(defvar stp-subtree-fetch t
  "This allows hashes to be resolved when installing or upgrading.")

(defun stp-git-fetch (remote)
  (db (exit-code output)
      (rem-call-process-shell-command (format "git fetch \"%s\"" remote))
    (unless (= exit-code 0)
      (error "git fetch failed: %s" output))))

(defun stp-git-maybe-fetch (remote version)
  (when stp-subtree-fetch
    (unless (stp-git-valid-remote-ref-p remote version)
      (stp-git-fetch remote)
      t)))

(cl-defun stp-git-install (pkg-info pkg-name remote version update &key branch (squash t))
  "Install the specified version of pkg-name from remote in
`stp-source-directory'."
  (let* ((git-root (stp-git-root stp-source-directory))
         (pkg-path (stp-absolute-path pkg-name))
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
                (setq pkg-info (stp-set-alist pkg-info pkg-name `((method . git)
                                                                  (remote . ,remote)
                                                                  (version . ,version)
                                                                  (update . ,update))))
                (when branch
                  (setq pkg-info (stp-set-attribute pkg-info pkg-name 'branch branch)))
                (stp-write-info pkg-info))
            (error "Failed to install %s as a git subtree: %s" pkg-name (s-trim output))))))))

(defvar stp-subtree-pull-fallback t
  "When this is non-nil and git subtree pull fails, attempt to uninstall the
package and install the new version instead.")

(cl-defun stp-git-upgrade (pkg-info pkg-name remote version &key (squash t))
  "Upgrade pkg-name in `stp-source-directory' to the specified version
from remote."
  (let* ((git-root (stp-git-root stp-source-directory))
         (pkg-path (stp-absolute-path pkg-name))
         (prefix (f-relative pkg-path
                             git-root)))
    (when (not (f-exists-p pkg-path))
      (error "%s does not exist" pkg-name))
    (rem-with-directory git-root
      ;; Upgrade package
      (let ((hash-p (stp-git-maybe-fetch remote version)))
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
                    (append (list (if hash-p
                                      ;; merging is done instead of pulling for
                                      ;; pulling for hashes because git subtree
                                      ;; pull does not work for hashes on
                                      ;; remotes.
                                      "merge"
                                    "pull")
                                  prefix)
                            (unless hash-p
                              (list remote))
                            (list version
                                  (if squash
                                      " --squash"
                                    "")))))
          (if (or (= exit-code 0)
                  ;; Sometimes git subtree pull fails. This can happen if the
                  ;; prefix has been changed since the subtree was created. In
                  ;; this case, we attempt to uninstall the package and install
                  ;; the new version instead.
                  (and (if stp-subtree-pull-fallback
                           (and (yes-or-no-p (format "git subtree merge/pull failed: %s. Uninstall and reinstall?" output))
                                (or stp-auto-commit
                                    (yes-or-no-p "Auto commits are disabled but an auto commit is required after uninstalling. Auto commit anyway?")))
                         (message "git subtree pull failed.")
                         nil)
                       (progn
                         ;; We do not want to commit, push or perform other
                         ;; actions. Those decisions are normally made in
                         ;; higher-level code (i.e. `stp-upgrade'). However, git
                         ;; subtree add will fail if there are uncommitted changes
                         ;; so we have to auto commit here. Refreshing the
                         ;; stp-list-buffer-name buffer is suppressed since that
                         ;; will be done by stp-upgrade (which calls this
                         ;; command).
                         (stp-uninstall pkg-name :do-commit t :refresh nil)
                         (let ((pkg-alist (stp-get-alist pkg-info pkg-name)))
                           (setf (alist-get 'version pkg-alist) version)
                           (stp-install pkg-name pkg-alist :refresh nil)
                           t))))
              (progn
                (if (stp-git-remote-head-p remote version)
                    ;; If we update to a head (i.e. a branch), update the branch
                    ;; parameter and store the current hash as the version. Since
                    ;; branches are constantly updated as more commits are pushed to
                    ;; the remote, storing a branch name is not particularly useful.
                    (let ((hash (car (rassoc version (stp-git-remote-hash-head-alist remote)))))
                      (setq pkg-info (stp-set-attribute pkg-info pkg-name 'version hash)
                            pkg-info (stp-set-attribute pkg-info pkg-name 'branch version)
                            pkg-info (stp-set-attribute pkg-info pkg-name 'update 'unstable)))
                  ;; For tags or hashes, use the tag or hash.
                  (setq pkg-info (stp-set-attribute pkg-info pkg-name 'version version))
                  (if (stp-git-remote-tag-p remote version)
                      ;; Tags do not have a branch to update from and are
                      ;; considered stable.
                      (setq pkg-info (stp-remove-attribute pkg-info pkg-name 'branch)
                            pkg-info (stp-set-attribute pkg-info pkg-name 'update 'stable))
                    ;; If there is a 'branch attribute when updating to a hash,
                    ;; leave it as is.
                    (setq pkg-info (stp-set-attribute pkg-info pkg-name 'update 'unstable))))
                (stp-write-info pkg-info))
            ;; This should never be reached since `stp-uninstall' and
            ;; `stp-install' raise errors when they fail.
            (error "Uninstalling and reinstalling %s failed: %s" pkg-name (s-trim output))))))))

(provide 'stp-git)
;;; stp-git.el ends here
