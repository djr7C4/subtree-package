;;; -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 David J. Rosenbaum <djr7c4@gmail.com>
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

(require 'f)
(require 'rem)
(require 's)
(require 'stp-bootstrap)
(require 'stp-utils)
(require 'timer)

(cl-defun stp-git-root (&key path (transform #'f-canonical))
  "Return the absolute path to the git repository containing PATH."
  (setq path (or (and path (funcall transform path)) default-directory))
  (let* ((default-directory path)
         (root (or (rem-run-command '("git" "rev-parse" "--show-toplevel"))
                   ;; Fallback for git repositories without working trees (e.g.
                   ;; those created with git clone --bare).
                   (rem-run-command '("git" "rev-parse" "--resolve-git-dir" ".")))))
    (and (> (length root) 0)
         (f-dir-p root)
         (f-slash (f-canonical root)))))

(defun stp-git-relative-path (path)
  "Return PATH relative to the git root."
  (rem-relative-path path (stp-git-root)))

(defmacro stp-with-git-root (&rest body)
  "Execute BODY in the git root for `stp-source-directory'."
  (declare (indent 0))
  `(let ((default-directory (stp-git-root :path stp-source-directory)))
     ,@body))

(def-edebug-spec stp-with-git-root t)

(defun stp-relative-path (pkg-name &optional top-level)
  "Return the path to PKG-NAME relative to `stp-source-directory'.

The return value always ends with a slash. If TOP-LEVEL is
non-nil, make the path relative to the root of the git repository
instead."
  (rem-slash (if top-level
                 (let ((path (f-join stp-source-directory pkg-name)))
                   (s-chop-prefix (rem-slash (f-full (stp-git-root :path path)))
                                  (f-full path)))
               pkg-name)))

(defun stp-split-current-package ()
  "Return a list containing the name of the package for the current
file and the relative path to the current file or directory
within that package."
  (stp-refresh-info)
  (let ((path (or buffer-file-name default-directory)))
    (db (pkg-name k)
        (->> (stp-info-names)
             (mapcar (lambda (pkg-name)
                       (list pkg-name
                             (->> path
                                  (s-matched-positions-all (regexp-quote (concat "/" (stp-relative-path pkg-name))))
                                  last
                                  caar))))
             (cl-find-if #'cadr))
      (list pkg-name (apply #'f-join (cddr (f-split (substring path k))))))))

(defun stp-git-tracked-p (path)
  "Determine if a file in a git repository is tracked."
  ;; This is needed to handle the case when path is a file in
  ;; `default-directory'. Without this, (f-dirname path) would be "./" which
  ;; will lead to the file not being found by git ls-files.
  (setq path (f-full path))
  (let* ((dir (f-dirname path))
         (file (f-relative path dir)))
    (unless (stp-git-root :path dir)
      (error "Not in a git repository"))
    (let ((default-directory dir))
      ;; `rem-call-process-shell-command' is more efficient than
      ;; `call-process-shell-command' because it does not load the shell's init
      ;; files.
      (eql (car (rem-call-process-shell-command (format "git ls-files --error-unmatch \"%s\"" file))) 0))))

(defun stp-git-remotes ()
  (--> (rem-run-command '("git" "remote" "-v") :error t)
       (s-split rem-newline-char-regexp it t)
       (mapcar (fn (-take 2 (s-split rem-positive-whitespace-regexp % t))) it)
       (mapcar (-partial #'apply #'cons) it)
       -uniq))

(defun stp-git-valid-remote-p (remote)
  "Determine if remote is a valid git repository."
  (and (stringp remote)
       (eql (car (rem-call-process-shell-command (format "git ls-remote -h '%s'" remote))) 0)))

(cl-defun stp-git-valid-remote-ref-p (remote rev &optional ask-p)
  ;; Check if rev is a ref on remote or if it is a hash that matches a ref on
  ;; remote.
  (and (or (member rev (stp-git-remote-tags remote t))
           (member rev (stp-git-remote-heads remote))
           ;; There is no way to check if hash exists on a remote (only refs) so
           ;; we ask the user.
           (and ask-p
                (yes-or-no-p (format "%s was not found in %s (this is normal for hashes). Continue?" rev remote))))
       t))

(cl-defun stp-git-valid-rev-p (path rev)
  "Check if REV is a valid git revision at the local PATH."
  (let ((default-directory path))
    (eql (car (rem-call-process-shell-command (format "git rev-parse --verify '%s'" rev))) 0)))

(defun stp-git-init (path)
  "Run \"git init\" on PATH."
  (let ((default-directory path))
    (unless (stp-git-root)
      (unless (eql (car (rem-call-process-shell-command "git init")) 0)
        (error "git init failed")))))

(cl-defun stp-git-add (path &key update)
  "Run \"git add\" on path. When UPDATE is non-nil, only add changes
to tracked files."
  (db (dir target)
      (if (f-dir-p path)
          ;; This allows path to be the top-level of a git repository.
          (list path ".")
        (list (f-dirname path) (f-filename path)))
    (let ((default-directory (f-full dir)))
      (rem-run-command (cl-list* "git"
                                 "add"
                                 target
                                 (rem-maybe-args "-u" update))
                       :error t))))

(defvar stp-git-synthetic-repos nil)

(defun stp-git-download-as-synthetic-repo (pkg-name remote)
  "Create a new git synthetic repository for PKG-NAME.

This is done by downloading REMOTE and adding it to the
repository. Return the path to the repository."
  (let (success
        (dir (make-temp-file pkg-name t)))
    (unwind-protect
        (progn
          (stp-download-elisp dir pkg-name remote)
          (stp-git-init dir)
          (stp-git-add dir)
          (let ((default-directory dir))
            (stp-git-commit ""))
          (setq success t))
      (unless success
        (f-delete dir t)))
    (push dir stp-git-synthetic-repos)
    dir))

(cl-defun stp-git-commit (msg &key (do-commit t))
  (when (stp-maybe-call do-commit)
    (when (stp-git-merge-conflict-p)
      (error "Committing is not possible due to %s."
             (if (> (length (stp-git-conflicted-files)) 1)
                 "merge conflicts"
               "a merge conflict")))
    (if (stp-git-clean-p)
        (stp-msg "There are no changes to commit. Skipping...")
      (rem-run-command (list "git" "commit" "--allow-empty-message" "-am" msg) :error t))))

(defvar stp-subtree-fetch t
  "This allows hashes to be resolved when installing or upgrading.")

(cl-defun stp-git-fetch (remote &key force refspec no-new-tags)
  ;; When no-new-tags is non-nil, download the tags objects but remove the
  ;; references to them in .git/refs/tags. Otherwise, the refs will become
  ;; cluttered with tags for remotes of packages. These tags won't have a clear
  ;; meaning on the local repository unless it is for this specific package. For
  ;; example, there could be a v2.0.0 tag for chatgpt-shell but v2.0.0 doesn't
  ;; have much meaning without even knowing which packages it is for. Git
  ;; doesn't seem to provide a way to do this so we copy .git/refs/tags
  ;; beforehand and then restore it after the fetch.
  (let* ((git-root (stp-git-root))
         (tags-dir-tmp (when no-new-tags
                         (make-temp-file "git-tags" t)))
         (tags-dir (car (-filter #'f-dir-p (list (f-join git-root ".git/refs/tags")
                                                 ;; Handle bare repositories.
                                                 (f-join git-root "refs/tags"))))))
    (unwind-protect
        (let ((cmd (append '("git" "fetch" "--atomic" "--tags")
                           (and force (list "--force"))
                           (list remote)
                           (and refspec (list refspec)))))
          (when no-new-tags
            (f-copy-contents tags-dir tags-dir-tmp))
          (rem-run-command cmd :error t)
          (when no-new-tags
            (f-delete tags-dir t)
            (f-move tags-dir-tmp tags-dir)))
      (when no-new-tags
        (f-delete tags-dir-tmp t)))))

(cl-defun stp-git-maybe-fetch (remote version &key force refspec no-new-tags)
  (when (and stp-subtree-fetch
             (not (stp-git-valid-remote-ref-p remote version)))
    (stp-git-fetch remote :force force :refspec refspec :no-new-tags no-new-tags)
    t))

(cl-defun stp-git-push (&key (do-push t) all tags)
  (when (stp-maybe-call do-push)
    (if (or all tags (stp-git-unpushed-p))
        (rem-run-command (append '("git" "push")
                                 (rem-maybe-args "--all" all
                                                 "--tags" tags))
                         :error t)
      (stp-msg "There is nothing to push. Skipping..."))))

(cl-defun stp-git-commit-push (msg &key (do-commit t) (do-push t) all tags)
  (when (stp-maybe-call do-commit)
    (stp-git-commit msg)
    ;; Pushing does not make sense if we did not commit earlier.
    (stp-git-push :do-push do-push :all all :tags tags)))

(cl-defun stp-git-reset (revision &key mode)
  (let* ((mode-flags '((soft . "--soft")
                       (mixed . "--mixed")
                       (hard . "--hard")))
         (mode-flag (and mode
                         (or (map-elt mode-flags mode)
                             (error "mode must be in %S" mode-flags)))))
    (rem-run-command (append (list "git" "reset")
                             (rem-maybe-args mode-flag mode-flag)
                             (list revision)))))

(defun stp-git-tag (tag revision)
  (rem-run-command (list "git" "tag" tag revision) :error t))

(cl-defun stp-git-status (&key keep-ignored keep-untracked)
  "Return a list of the status of each file in the repository.

Each status is a list containing three or four elements. The
first element is the character status code for the index used in
version 1 of the git porcelain format (see the manual for
git-status) and the second element is the character status code
for the worktree. The third is the file name. When a file is
renamed or copied, there is also a fourth element that indicates
the new name."
  (let ((output (rem-run-command '("git" "status" "--porcelain") :error t)))
    (cl-remove-if (lambda (status)
                    (db (index-status worktree-status &rest args)
                        status
                      (and (string= index-status worktree-status)
                           (member index-status
                                   (append (and (not keep-ignored) (list "!"))
                                           (and (not keep-untracked) (list "?")))))))
                  (mapcar (lambda (line)
                            ;; The first two characters can be spaces which have
                            ;; a specific meaning and should not be used to
                            ;; split the strings.
                            (cl-list* (substring line 0 1)
                                      (substring line 1 2)
                                      (and (mapcar #'s-trim (s-split "->" (substring line 2))))))
                          (s-split "\n" output t)))))

(defun stp-git-clean-p ()
  "Determine if the git repository has uncommitted changes."
  (and (not (stp-git-status)) t))

(defun stp-git-unpushed-p ()
  (let* ((branch (stp-git-current-branch))
         (upstream (stp-git-upstream-branch)))
    (and branch
         upstream
         (not (string= (rem-run-command (list "git" "cherry" upstream branch) :error t) "")))))

(defun stp-git-conflicted-files ()
  "Return the list of files with merge conflicts."
  (->> (stp-git-status)
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

(defun stp-git-modified-files ()
  (mapcar (lambda (entry) (caddr entry)) (stp-git-status)))

(defun stp-git-tree-modified-p (path)
  "Determine if any files have been modified since the last commit."
  (setq path (f-canonical path))
  (let ((default-directory (stp-git-root :path path)))
    (cl-some (lambda (file)
               (setq file (f-canonical file))
               (or (f-same-p path file)
                   (f-ancestor-of-p path file)))
             (stp-git-modified-files))))

(defun stp-git-upstream-branch (&optional branch)
  "Get the upstream branch of BRANCH if it exists.

Otherwise, return nil. BRANCH defualts to the current branch."
  (setq branch (or branch ""))
  (rem-run-command (list "git" "rev-parse" "--abbrev-ref" (format "%s@{upstream}" branch))))

;; Based on `magit-get-current-branch'.
(defun stp-git-current-branch ()
  "Return the current branch or nil if HEAD is detached."
  (rem-run-command '("git" "symbolic-ref" "--short" "HEAD")))

;; Based on `magit-get-push-remote'.
(defun stp-git-push-target (&optional branch)
  (setq branch (or branch (stp-git-current-branch)))
  ;; git config --get returns a non-zero exit status when the value does not
  ;; exist. This will result in `rem-run-command' returning nil.
  (let ((push-default (rem-run-command '("git" "config" "--get" "remote.pushDefault"))))
    (or push-default
        (let ((push-remote (rem-run-command (list "git" "config" "--get" (s-join "." (list "branch" branch "pushRemote"))))))
          (when (equal push-remote "")
            (setq push-remote nil))
          push-remote))))

(defun stp-git-remote-url (remote)
  (rem-run-command (list "git" "remote" "get-url" remote) :error t))

(defvar stp-git-abbreviated-hash-length 7)

(defun stp-git-abbreviate-hash (hash)
  (s-left stp-git-abbreviated-hash-length hash))

(defun stp-git-tree (path &optional rev)
  "Determine the hash of the git tree at PATH for revision REV.

If there is no git tree at PATH then nil will be returned."
  (unless (f-dir-p path)
    (error "The directory %s does not exist" path))
  (setq path (f-canonical path)
        rev (or rev "HEAD"))
  (let* ((default-directory (stp-git-root :path path))
         ;; Git will show the children of the directory even when -d is
         ;; specified when the directory ends with a slash.
         (rel-path (rem-no-slash (stp-git-relative-path path))))
    (when (f-same-p default-directory rel-path)
      (error "Cannot determine the hash of the top-level git repository"))
    (let ((output (rem-run-command (list "git" "ls-tree" "-d" rev "--object-only" rel-path) :error t)))
      (and (not (string= output "")) output))))

(defun stp-git-tree-paths (path &optional rev)
  "Compute the paths for the contents of the git tree at PATH.

This is done for revision REV when it is non-nil."
  (unless (f-dir-p path)
    (error "The directory %s does not exist" path))
  (setq path (f-canonical path)
        rev (or rev "HEAD"))
  (let* ((default-directory (stp-git-root :path path))
         (rel-path (stp-git-relative-path path))
         (cmd (list "git" "ls-tree" "-r" rev "--name-only" rel-path)))
    (s-split "\n" (rem-run-command cmd :error t) t)))

(defun stp-git-subtree-commit-message (path &optional format)
  "Return the stp-msg for the last local commit by git subtree.

This is different from the remote commit that was merged when
--squash is used."
  (unless (f-dir-p path)
    (error "The directory %s does not exist" path))
  (let* ((default-directory path)
         (rel-path (rem-no-slash (rem-relative-path path (stp-git-root))))
         (grep-target (format "^[ \t]*git-subtree-dir:[ \t]*%s[ \t]*$" rel-path))
         (cmd (append (list "git" "log" "--grep" grep-target "-n" "1")
                      (and format (list (format "--format=%s" format))))))
    (rem-run-command cmd)))

(defun stp-git-subtree-commit (path)
  "Get the last remote commit merged into the subtree at PATH."
  (let ((output (rem-empty-nil (stp-git-subtree-commit-message path) #'s-trim)))
    (and output
         (save-match-data
           (string-match "^[ \t]*git-subtree-split:[ \t]*\\([A-Fa-f0-9]+\\)[ \t]*" output)
           (match-string 1 output)))))

(defun stp-git-subtree-tree (path)
  "Get the last git tree merged into the subtree at PATH."
  (rem-empty-nil (stp-git-subtree-commit-message path "%T") #'s-trim))

(defun stp-git-rev-parse (path rev)
  (let ((default-directory path))
    (rem-run-command (list "git" "rev-parse" rev))))

(defun stp-git-commit-tree (path rev)
  "Get the git tree associated with REV in the repository at PATH."
  (stp-git-rev-parse path (format "%s^{tree}" rev)))

(defun stp-git-subtree-modified-p (path &optional remote rev)
  "Test if the git subtree at PATH has been modified by the user.

This means outside of changes made by git subtree add and merge
commands. Return a list containing the hash of the current tree
and the hash of the tree that was installed. If REMOTE and REV
are provided then they will be used to compute the hash for the
subtree in the event that it cannot be determined from git log.
This occurs for example when the subtree was not actually
installed as a git subtree."
  (let ((tree (or (stp-git-tree path)
                  (error "Unable to find the git tree for %s" path)))
        (last-tree (or (stp-git-subtree-tree path)
                       (and remote
                            rev
                            ;; This is slower but should work even when the
                            ;; subtree was not installed using git subtree add.
                            ;; It compares the actual git tree (and by extension
                            ;; their contents since trees with differing
                            ;; contents) will have different hashes.
                            (progn
                              (stp-git-fetch remote :no-new-tags t)
                              ;; Some revs (e.g. tags) won't be available
                              ;; locally even after a fetch since we used
                              ;; :no-new-tags t. We convert them to hashes to
                              ;; avoid issues. If we didn't supply :no-new-tags
                              ;; t existing tags with the same name might get
                              ;; clobbered.
                              (stp-git-commit-tree path (stp-git-remote-rev-to-hash remote rev))))
                       (error "Unable to find the merged git subtree"))))
    (and (not (string= tree last-tree))
         (list tree last-tree))))

(defun stp-git-subtree-p (path)
  (and (stp-git-subtree-commit path) t))

(defun stp-git-head (&optional path)
  (stp-git-remote-head (stp-git-root :path (or path stp-source-directory))))

(defun stp-git-diff (&optional hashes)
  (rem-run-command (cl-list* "git" "diff" hashes) :error t))

(defvar stp-git-diff-buffer-name "*stp-git-diff*")
;; This is a change
(defun stp-git-show-diff (&optional hashes)
  ;; (stp-git-show-diff hash new-hash): show the changes from hash to new-hash
  ;; (stp-git-show-diff hash): show the changes from hash to the index
  ;; (stp-git-show-diff): show the changes from the index to the working tree
  (let ((buf (get-buffer-create stp-git-diff-buffer-name))
        (diff (stp-git-diff hashes)))
    (with-current-buffer buf
      (read-only-mode 0)
      (erase-buffer)
      (insert diff)
      (goto-char (point-min))
      (read-only-mode 1)
      (diff-mode))
    (pop-to-buffer buf)))

;; This function exists to facilitate memoization.
(defun stp-git-remote-hash-alist-basic (remote)
  (rem-run-command (list "git" "ls-remote" remote) :error t :nostderr t))

(cl-defun stp-git-remote-hash-alist (remote &key (prefixes nil prefixes-supplied-p))
  "Return an alist that maps hashes to refs.

If supplied, PREFIXES is a list of allowed prefixes. Matching
prefixes are removed from the refs. By default all refs are
returned."
  ;; This function should not be passed an invalid remote and this check has a
  ;; significant performance penalty even with caching.
  ;; (unless (stp-git-valid-remote-p remote)
  ;;   (error "%s is not a valid remote" remote))
  ;; Handle empty repositories that do not have any tags.
  (let ((output (stp-git-remote-hash-alist-basic remote)))
    (unless (equal output "")
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
                               (s-split "\n" output)))))))

(defun stp-git-remote-hash-tag-alist (remote)
  "Return an alist that maps hashes to tags."
  (stp-git-remote-hash-alist remote :prefixes '("refs/tags/")))

(defun stp-git-remote-tags (remote &optional keep-dereferences)
  (let ((tags (mapcar #'cdr (stp-git-remote-hash-tag-alist remote))))
    (if keep-dereferences
        tags
      (cl-remove-if (-partial #'s-ends-with-p "^{}") tags))))

(defun stp-git-remote-tag-p (remote rev)
  (member rev (stp-git-remote-tags remote t)))

(defun stp-git-remote-hash-head-alist (remote)
  "Return an alist that maps hashes to heads."
  ;; Manually add HEAD instead of using the branch refs/heads/HEAD. This branch
  ;; should not exist as it is likely to create confusion but some repositories
  ;; may have created it by mistake.
  (cons (cons (stp-git-remote-head remote) "HEAD")
        ;; A few repositories have a branch that is named HEAD. This should be
        ;; ignored.
        (map-remove (lambda (_hash head)
                      (string= head "HEAD"))
                    (stp-git-remote-hash-alist remote :prefixes '("refs/heads/")))))

(defun stp-git-remote-head (remote)
  "Return HEAD for REMOTE."
  (car (rassoc "HEAD" (stp-git-remote-hash-alist remote))))

(defun stp-git-remote-heads (remote)
  (mapcar #'cdr (stp-git-remote-hash-head-alist remote)))

(defun stp-git-remote-head-p (remote ref)
  (member ref (stp-git-remote-heads remote)))

;; Note that this will not work will minimal copies of a repositories created
;; using CLI options such as those used in `stp-git-count-remote-commits'.
(cl-defun stp-git-hashes (path rev &key max)
  "Return a list of the hashes reachable from REV at PATH.

Hashes are sorted starting with the most recent. If REV is nil
then HEAD will be used. If REV is t then all hashes will be
returned. If MAX is non-nil then no more than MAX hashes will be
returned."
  (setq rev (cond
             ((null rev)
              "HEAD")
             ((eq rev t)
              "--all")
             (t
              rev)))
  (let ((default-directory path)
        (cmd (append '("git" "reflog" "show" rev)
                     (and max (list "-n" (number-to-string max)))
                     (list "--pretty='%%H'"))))
    (s-split "\n" (rem-run-command cmd :error t) t)))

(defun stp-git-remote-rev-to-hash (remote rev)
  "Convert REV to a hash if it isn't one already.

Refs that do not match any hash will remain unchanged."
  (or (car (or (rassoc rev (stp-git-remote-hash-head-alist remote))
               ;; Be aware that some remotes will not return hashes for
               ;; dereferenced tags in which case it will not be possible to
               ;; determine the hash for the tag. This is why there is a
               ;; fallback.
               (rassoc (stp-git-tag-append-dereference rev) (stp-git-remote-hash-tag-alist remote))
               (rassoc rev (stp-git-remote-hash-tag-alist remote))))
      rev))

(defun stp-git-remote-head-to-hash (remote rev)
  "If REV is a head, convert it to a hash.

Otherwise, return REV."
  (or (car (rassoc rev (stp-git-remote-hash-head-alist remote)))
      rev))

(defun stp-git-remote-tag-to-hash (remote rev)
  "If REV is a tag, convert it to a hash.

Otherwise, return REV."
  (or (car (rassoc rev (stp-git-remote-hash-tag-alist remote)))
      rev))

(defun stp-git-remote-hash-to-head (remote rev)
  "If REV is a hash that corresponds to a head, return the head.

Otherwise, return REV."
  (or (map-elt (stp-git-remote-hash-head-alist remote) rev)
      rev))

(defun stp-git-rev-to-hash (path rev)
  (let ((default-directory path))
    (stp-git-remote-rev-to-hash "." rev)))

(defun stp-git-head-to-hash (path rev)
  (let ((default-directory path))
    (stp-git-remote-head-to-hash "." rev)))

(defun stp-git-tag-to-hash (path rev)
  (let ((default-directory path))
    (stp-git-remote-tag-to-hash "." rev)))

(defun stp-git-hash-to-head (path rev)
  (let ((default-directory path))
    (stp-git-remote-hash-to-head "." rev)))

(defun stp-git-tag-strip-dereference (tag)
  "When tag is non-nil, remove the ^{} following a tag object if it
is present."
  ;; A tag followed by ^{} means to dereference the tag until a commit is
  ;; reached.
  (s-chop-suffix "^{}" tag))

(defun stp-git-tag-append-dereference (tag)
  "Append ^{} to TAG unless it already has that suffix."
  (if (s-ends-with-p "^{}" tag)
      tag
    (concat tag "^{}")))

(defun stp-git-remote-dereferencable-tag-p (remote rev)
  (car (rassoc (stp-git-tag-append-dereference rev) (stp-git-remote-hash-tag-alist remote))))

(defun stp-git-remote-rev-to-tag (remote rev &optional keep-dereference)
  "If REV is a hash that corresponds to a tag, return the tag.

Otherwise, return REV."
  (or (let ((tag (map-elt (stp-git-remote-hash-tag-alist remote) rev)))
        (if keep-dereference
            tag
          (stp-git-tag-strip-dereference tag)))
      rev))

(defun stp-git-rev-to-tag (path rev &optional keep-dereference)
  (let ((default-directory path))
    (stp-git-remote-rev-to-tag "." rev keep-dereference)))

(defun stp-git-hash= (hash hash2)
  (and (>= (length hash) 6)
       (>= (length hash2) 6)
       (or (s-prefix-p hash hash2)
           (s-prefix-p hash2 hash))))

(cl-defun stp-git-count-commits (path rev rev2 &key (handle-unrelated t))
  "Count the number of commits from REV to REV2.

More precisely, compute the number of commits M and N in
REV..REV2 and REV2..REV in the local git repository at PATH.

If M is non-zero and N is zero, then return M. If M is zero and N
is non-zero, return -N (this represents going backwards from REV2
to REV). If both are non-zero, return a list of the form (M N).

If HANDLE-UNRELATED is non-nil, then nil will be returned if REV
and REV2 do not share a common ancestor."
  ;; This has a significant performance penalty. `rem-run-command' will produce
  ;; an error below anyway if a revision is invalid.
  ;;
  ;; (unless (stp-git-valid-rev-p path rev)
  ;;   (error "%s is not a valid ref or hash for %s" rev path))
  ;; (unless (stp-git-valid-rev-p path rev2)
  ;;   (error "%s is not a valid ref or hash for %s" rev2 path))
  (let ((default-directory path))
    (cl-flet ((common-ancestor-exists-p (rev rev2)
                (let ((cmd (list "git" "merge-base" rev rev2)))
                  ;; git merge-base returns 0 when a common ancestor exists and
                  ;; 1 when it does not. Other statuses indicate errors.
                  (= (rem-run-command cmd
                                      :return 'exit-code
                                      :validate (lambda (exit-code _)
                                                  (member exit-code '(0 1)))
                                      :error t)
                     0)))
              (count-commits-forward (rev rev2)
                ;; "--since" (number-to-string (min (stp-git-timestamp path rev)
                ;; (stp-git-timestamp path rev2))) was previously used to ignore
                ;; commits that are older than either revision. This was done
                ;; because there can be commits that are not excluded just by
                ;; the range if one revision is from a branch that was never
                ;; merged upstream. This can occur for example when a local dev
                ;; branch is used that pull requests are merged into before they
                ;; are approved upstream.
                ;;
                ;; However, it can also exclude old commits that were merged
                ;; into such a dev branch recently and should thus not be
                ;; excluded. A better system is to delete and recreated such a
                ;; dev branch from main instead of reusing it.
                (let ((cmd (list "git" "rev-list" "--count" (format "%s..%s" rev rev2))))
                  (string-to-number (rem-run-command cmd :error t)))))
      (let ((m (count-commits-forward rev rev2))
            (n (count-commits-forward rev2 rev)))
        (cond
         ((and (= m 0) (= n 0))
          0)
         ((= n 0)
          m)
         ((= m 0)
          (- n))
         ;; When one or both m and n are zero rev and rev2 must have a common
         ;; ancestor so we don't need to check for a common ancestor in the
         ;; above cases regardless of the value of handle-unrelated. This will
         ;; be faster in most cases since usually m and n will not both be
         ;; non-zero (this will typically only happen when there is a fork
         ;; installed).
         ((or (not handle-unrelated) (common-ancestor-exists-p rev rev2))
          (list m n))
         ;; When no common ancestor exists and handle-unrelated is nil, return
         ;; nil.
         (t
          nil))))))

(defun stp-git-clone (remote path)
  (rem-run-command (list "git" "clone" remote path) :error t))

(defvar stp-git-cache-directory (f-join user-emacs-directory "stp/cache/git-repos/"))

(defun stp-git-minimal-clone (remote path &optional branch)
  ;; Make the call to git clone as lightweight as possible.
  (let ((cmd (append '("git" "clone" "--bare" "--no-checkout" "--filter=blob:none")
                     (and branch (format " --single-branch --branch '%s'" branch))
                     (list remote path))))
    (rem-run-command cmd :error t)))

(defun stp-git-cache-hash-directory (remote)
  (when (f-dir-p remote)
    (setq remote (f-canonical remote)))
  ;; This doesn't work for very long remotes (which can occur when remote is
  ;; actually several remotes that were joined together by
  ;; `stp-git-cached-repo-path').
  ;;
  ;; (->> remote
  ;;      ;; URLs and local filenames are converted to hex to avoid
  ;;      ;; conflicts. If we only replaced /'s with -'s for example,
  ;;      ;; local files and URLs could theoretically resolve to the
  ;;      ;; same cached path.
  ;;      (mapcar (lambda (char)
  ;;                (format "%02x" char)))
  ;;      (s-join "-"))
  (secure-hash 'sha512 remote))

(defun stp-git-cached-repo-path (remote)
  ;; When there is a list of remotes, use a combination of all the remote URLs
  ;; for caching.
  (let ((id (if (listp remote)
                (s-join "|" remote)
              remote)))
   (f-join stp-git-cache-directory (stp-git-cache-hash-directory id))))

(defvar stp-git-cached-repo-timestamp-suffix "-timestamp")

(defun stp-git-cached-repo-timestamp-path (path)
  (f-join stp-git-cache-directory (format "%s%s" path stp-git-cached-repo-timestamp-suffix)))

(defun stp-git-ensure-cached-repo (remotes &optional branch)
  (if (listp remotes)
      ;; Fetch in reverse order so that branches and tags in earlier remotes
      ;; take precedence.
      (setq remotes (reverse remotes))
    (setq remotes (list remotes)))
  (unless (f-dir-p stp-git-cache-directory)
    (f-mkdir-full-path stp-git-cache-directory))
  (let* ((path (stp-git-cached-repo-path remotes))
         (tpath (stp-git-cached-repo-timestamp-path path)))
    ;; Ensure that the cached repository has been created.
    (unless (f-dir-p path)
      (stp-git-minimal-clone (car remotes) path branch)
      (setq remotes (cdr remotes)))
    ;; Fetch and update all branches from all remotes.
    (let ((default-directory path))
      (cl-dolist (remote remotes)
        ;; :force t is required in case a branch is deleted upstream. We don't
        ;; want :no-new-tags t because the tags should be kept in refs/tags for
        ;; cached repos since they correspond to a single package.
        (stp-git-fetch remote :force t :refspec "*:*")))
    (f-write (format "%f" (float-time)) 'utf-8 tpath)
    path))

(defvar stp-git-stale-cached-repo-interval (timer-duration "1 year")
  "Cached repos can be deleted if they have not been used this long.")

(defun stp-git-delete-stale-cached-repos ()
  (when (f-dir-p stp-git-cache-directory)
    (f-entries stp-git-cache-directory
               (lambda (file)
                 (and (s-ends-with-p stp-git-cached-repo-timestamp-suffix file)
                      (let ((updated (string-to-number (f-read file 'utf-8))))
                        (when (> (- (float-time) updated)
                                 stp-git-stale-cached-repo-interval)
                          (f-delete (s-chop-suffix stp-git-cached-repo-timestamp-suffix file) t)
                          (f-delete file))))))))

(cl-defun stp-git-count-remote-commits (remotes rev rev2)
  "This is `stp-git-count-commits' for remote repositories.

REMOTES can be either a single repository or a list of multiple
repositories."
  ;; branch is ignored because it does not save much space and branches are not
  ;; known for stable git packages which prevents using it there anyway. This
  ;; would result in multiple cached versions of the same repository if it was
  ;; changed from stable to unstable for example.
  (let ((path (stp-git-ensure-cached-repo remotes)))
    (stp-git-count-commits path rev rev2)))

(defun stp-git-timestamp (path rev)
  "Return the UNIX timestamp for when REV was commited at PATH."
  (let ((default-directory path)
        (cmd (list "git" "show" "--no-patch" "--format=%ct" (format "%s^{commit}" rev))))
    ;; Pipe to /dev/null to suppress warnings about ambiguous ref or hashs.
    ;; These can occur when the git repository contains a branch or tag called
    ;; HEAD.
    ;;
    ;; The ^{commit} syntax forces git to show the commit object pointed to
    ;; by a tag rather than the tag.
    (string-to-number (rem-run-command cmd :error t :nostderr t))))

(defun stp-git-remote-timestamp (remote rev)
  "This is similar to `stp-git-timestamp' except that it works with
remote repositories."
  (let ((path (stp-git-ensure-cached-repo remote)))
    (stp-git-timestamp path rev)))

(provide 'stp-git-utils)
;;; stp-git-utils.el ends here
