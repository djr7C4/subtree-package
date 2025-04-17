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

(require 'cl-lib)
(require 's)
(require 'stp-utils)
(require 'stp-git-utils)

(defvar stp-auto-commit)
(declare-function stp-reinstall "stp")

(defun stp-git-subtree-package-hash (pkg-name)
  (let ((pkg-path (stp-canonical-path pkg-name)))
    (stp-git-subtree-hash pkg-path)))

(defun stp-git-subtree-package-p (pkg-name)
  "Determine if there is a git subtree for this package."
  (and (stp-git-subtree-package-hash pkg-name) t))

(defun stp-normalize-version (pkg-name remote version)
  ;; If version is a hash, it might be shortened if the user entered it
  ;; manually. In this case, we replace it with the full hash from the installed
  ;; subtree.
  (if (stp-git-valid-remote-ref-p remote version)
      version
    (stp-git-subtree-package-hash pkg-name)))

(defun stp-git-subtree-version (pkg-name)
  "Determine the version and the update type of the package that was
installed at the subtree. Use a tag if one is available;
otherwise, use the hash."
  (let* ((pkg-name (stp-name pkg-name)))
    (let-alist (stp-get-alist pkg-name)
      (if (stp-git-valid-remote-p .remote)
          (let ((cur-hash (stp-git-subtree-package-hash pkg-name))
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
                        (< (cl-position v1 order :test #'string=)
                           (cl-position v2 order :test #'string=)))
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

(defun stp-git-latest-stable-version (remote)
  (let ((path (stp-git-ensure-cached-repo remote)))
    (stp-git-remote-latest-tag path)))

(defun stp-git-latest-unstable-version (remote ref)
  (let ((path (stp-git-ensure-cached-repo remote)))
    (if (string= ref "HEAD")
        (stp-git-remote-head path)
      (car (rassoc ref (stp-git-remote-hash-head-alist path))))))

(defun stp-git-version-upgradable-p (count-to-stable count-to-unstable update)
  (if (eq update 'stable)
      (and count-to-stable (> count-to-stable 0) t)
    (and count-to-unstable (> count-to-unstable 0) t)))

(cl-defun stp-git-install (pkg-name remote version update &key branch (squash t) (set-pkg-info t))
  "Install the specified version of pkg-name from remote in
`stp-source-directory'."
  (let* ((git-root (stp-git-root stp-source-directory))
         (pkg-path (stp-canonical-path pkg-name))
         (prefix (f-relative pkg-path git-root)))
    (when (f-exists-p pkg-path)
      (error "%s already exists" pkg-name))
    ;; Clone the remote repository as a squashed subtree.
    (let ((default-directory git-root))
      ;; Install the package.
      (let ((hash-p (stp-git-maybe-fetch remote version)))
        (db (exit-code output)
            (rem-call-process-shell-command
             (apply #'format
                    (concat "git subtree add --prefix \"%s\"%s "
                            ;; When the version is a hash, don't provide a
                            ;; remote to git subtree add. This forces git to
                            ;; look for the commit locally instead which is
                            ;; possible since we previously ran git fetch.
                            (if hash-p
                                " "
                              "\"%s\" ")
                            "\"%s\"")
                    (append (list prefix
                                  (if squash
                                      " --squash"
                                    ""))
                            (unless hash-p
                              (list remote))
                            (list version))))
          (if (= exit-code 0)
              ;; If installation was successful, add the information for the package
              (when set-pkg-info
                (setq version (stp-normalize-version pkg-name remote version))
                (stp-set-alist pkg-name
                               `((method . git)
                                 (remote . ,remote)
                                 (version . ,version)
                                 (update . ,update)))
                (when branch
                  (stp-set-attribute pkg-name 'branch branch)))
            (error "Failed to install %s as a git subtree: %s" pkg-name (s-trim output))))))))

(defvar stp-subtree-pull-fallback t
  "When this is non-nil and git subtree pull fails, attempt to uninstall the
package and install the new version instead.")

(cl-defun stp-git-upgrade (pkg-name remote version &key (squash t) (set-pkg-info t))
  "Upgrade pkg-name in `stp-source-directory' to the specified version
from remote."
  (let* ((git-root (stp-git-root stp-source-directory))
         (pkg-path (stp-canonical-path pkg-name))
         (prefix (f-relative pkg-path git-root)))
    (unless (f-exists-p pkg-path)
      (error "%s does not exist" pkg-name))
    (let ((default-directory git-root))
      ;; Upgrade package
      (let* ((hash-p (stp-git-maybe-fetch remote version))
             (action (if hash-p
                         ;; merging is done instead of pulling for
                         ;; hashes because git subtree pull does
                         ;; not work for hashes on remotes.
                         "merge"
                       "pull"))
             (version-hash (stp-git-ref-to-hash remote version)))
        (when (stp-git-hash= (stp-git-subtree-package-hash pkg-name) version-hash)
          (user-error "Commit %s of %s is already installed"
                      (if (stp-git-hash= version version-hash)
                          (stp-git-abbreviate-hash version-hash)
                        (format "%s (%s)" (stp-git-abbreviate-hash version-hash) version))
                      pkg-name))
        (db (exit-code output)
            (rem-call-process-shell-command
             (apply #'format
                    (concat "git subtree %s --prefix \"%s\"%s "
                            ;; When the version is a hash, don't provide a
                            ;; remote since git subtree merge doesn't need one.
                            (if hash-p
                                " "
                              "\"%s\" ")
                            "\"%s\"")
                    (append (list action
                                  prefix
                                  (if squash
                                      " --squash"
                                    ""))
                            (unless hash-p
                              (list remote))
                            (list version))))
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
            (stp-reinstall pkg-name version))
           ;; Handle git subtree merge/pull errors and when the user chose not
           ;; to proceed with uninstalling and reinstalling the package.
           (t
            (error "Uninstalling and reinstalling %s failed: %s" pkg-name (s-trim output))))
          ;; If we get this far it means that either the merge succeeded or
          ;; there was a merge conflict which will be resolved manually by the
          ;; user. Either way, we update the package database.
          (when set-pkg-info
            (if (stp-git-remote-head-p remote version)
                ;; If we update to a head (i.e. a branch), update the branch
                ;; parameter and store the current hash as the version. Since
                ;; branches are constantly updated as more commits are pushed to
                ;; the remote, storing a branch name does not make sense.
                (progn
                  (stp-set-attribute pkg-name 'version version-hash)
                  (stp-set-attribute pkg-name 'branch version)
                  (stp-set-attribute pkg-name 'update 'unstable))
              ;; For tags or hashes, use the tag or hash.
              (setq version (stp-normalize-version pkg-name remote version))
              (stp-set-attribute pkg-name 'version version)
              (if (stp-git-remote-tag-p remote version)
                  ;; Tags do not have a branch to update from and are considered
                  ;; stable.
                  (progn
                    (stp-delete-attribute pkg-name 'branch)
                    (stp-set-attribute pkg-name 'update 'stable))
                ;; If there is a 'branch attribute when updating to a hash,
                ;; leave it as is.
                (stp-set-attribute pkg-name 'update 'unstable)))))))))

(provide 'stp-git)
;;; stp-git.el ends here
