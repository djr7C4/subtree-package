;;; stp.el --- Manage packages as git subtrees -*- lexical-binding: t; -*-
;; Copyright (C) 2025 David J. Rosenbaum

;; Author: David J. Rosenbaum <djr7c4@gmail.com>
;; Keywords: TODO
;; URL: https://github.com/djr7C4/subtree-package
;; Version: TODO
;; Package-Requires: ()

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

(require 'align)
(require 'async nil t)
(require 'find-lisp)
(require 'info)
(require 'queue nil t)
(require 'stp-archive)
(require 'stp-bootstrap)
(require 'stp-utils)
(require 'stp-elpa)
(require 'stp-git)
(require 'stp-url)
(require 'timer)
(require 'url-handlers)

(defvar stp-memoized-functions '(stp-refresh-info stp-git-download-as-synthetic-repo stp-git-ensure-cached-repo stp-git-valid-remote-p stp-git-remote-hash-alist-basic stp-git-remote-hash-alist stp-git-valid-ref-p stp-git-timestamp stp-git-tree stp-elpa-version-url-alist stp-achive-get-descs))

(defvar stp-memoization-active nil)

(defmacro stp-with-memoization (&rest body)
  "Evaluate BODY with memoization active for expensive functions.
Cached results are only retained while within the scope of this
macro. This allows functions that would otherwise make many
duplicate queries to remote Git repositories to only make one of
each type per interactive command."
  (declare (indent 0))
  (with-gensyms (memoization-active-orig)
    `(let ((,memoization-active-orig stp-memoization-active)
           (stp-memoization-active t))
       (unwind-protect
           (progn
             (unless ,memoization-active-orig
               (mapc (-rpartial #'memoize nil) stp-memoized-functions))
             ,@body)
         (unless ,memoization-active-orig
           (mapc (-rpartial #'f-delete t) stp-git-synthetic-repos)
           (mapc #'memoize-restore stp-memoized-functions))))))

(def-edebug-spec stp-with-memoization t)

(defvar stp-normalize-versions nil
  "Indicates if versions should be printed in the same format by STP
commands regardless of the specific format used for versions by
the project.")

(defun stp-abbreviate-remote-version (pkg-name method remote version)
  "Abbreviate long hashes to make them more readable. Other versions
are not abbreviated."
  (cond
   ((and (eq method 'git) (not (stp-git-valid-remote-ref-p remote version)))
    (stp-git-abbreviate-hash version))
   (stp-normalize-versions
    (stp-normalize-version pkg-name remote version))
   (t
    version)))

(defvar stp-remote-history nil)

(defun stp-read-remote (prompt &optional default)
  "Read any type of remote."
  (-> prompt
      (stp-read-remote-with-predicate
       (lambda (remote)
         (-any-p (lambda (predicate)
                   (funcall predicate remote))
                 stp-methods-order))
       default
       'stp-remote-history)
      stp-normalize-remote))

(cl-defun stp-read-remote-or-archive (prompt &key pkg-name default-remote (prompt-prefix ""))
  "Read a package name and remote of any type or a package archive.
When the input is ambiguous and could be package name or a local
path, it will be treated as a package name unless it contains a
slash. Return a cons cell the contains the package name and the
remote or archive. Archives are represented as symbols."
  (stp-archive-ensure-loaded)
  (let* ((archive-names (stp-archive-package-names))
         (name-or-remote (stp-comp-read-remote prompt archive-names :default default-remote :normalize nil)))
    (if (member name-or-remote archive-names)
        (progn
          ;; If the user chose a package name, find remotes from
          ;; `package-archive-contents' and allow the user to choose one.
          (setq pkg-name name-or-remote)
          (let* ((archives (stp-archives pkg-name))
                 (archive-alist (mapcar (lambda (archive)
                                          (cons (format "%s (package archive)" archive)
                                                (intern archive)))
                                        archives))
                 (remotes (stp-archive-find-remotes pkg-name))
                 (remote-or-archive (stp-comp-read-remote "Remote or archive: "
                                                          (append remotes (mapcar #'car archive-alist))
                                                          :default (car remotes))))
            (cons pkg-name (or (map-elt archive-alist remote-or-archive)
                               remote-or-archive))))
      ;; Otherwise the user chose a remote so prompt for its package name.
      (let ((remote (stp-normalize-remote name-or-remote)))
        (cons (or pkg-name (stp-read-name (stp-prefix-prompt prompt-prefix "Package name: ") (stp-default-name remote)))
              remote)))))

(cl-defun stp-read-package (&key pkg-name pkg-alist (prompt-prefix ""))
  (plet* ((`(,pkg-name . ,remote) (stp-read-remote-or-archive (stp-prefix-prompt prompt-prefix "Package name or remote: ")
                                                              :pkg-name pkg-name
                                                              :default-remote (map-elt pkg-alist 'remote)))
          (method (stp-remote-method remote)))
    (let (version update branch)
      (cl-ecase method
        (git
         (unless (stp-git-valid-remote-p remote)
           (user-error (stp-prefix-prompt prompt-prefix "Invalid git repository (or host is down): %s") remote))
         (unless update
           (setq update (stp-git-read-update (stp-prefix-prompt prompt-prefix "Update policy: ") (map-elt pkg-alist 'update))))
         (when (and (eq update 'unstable)
                    (not branch))
           (setq branch (stp-git-read-branch (stp-prefix-prompt prompt-prefix "Branch: ") remote (map-elt pkg-alist 'branch))))
         (unless version
           (setq version (stp-git-read-version (stp-prefix-prompt prompt-prefix "Version: ") remote :extra-versions (list (map-elt pkg-alist 'version) branch) :default (map-elt pkg-alist 'version))))
         `(,pkg-name
           (method . ,method)
           (remote . ,remote)
           (version . ,version)
           (update . ,update)
           (branch . ,branch)))
        (archive
         `(,pkg-name
           (method . ,method)
           (remote . ,remote)))
        ((elpa url)
         (unless (or (and (string-match-p rem-strict-url-regexp remote)
                          (url-file-exists-p remote))
                     ;; Allow local files too.
                     (f-exists-p remote))
           (user-error (stp-prefix-prompt prompt-prefix "Invalid URL (or host is down): %s") remote))
         (unless version
           (cl-ecase method
             (elpa (setq version (stp-elpa-read-version (stp-prefix-prompt prompt-prefix "Version: ") pkg-name remote)))
             (url (setq version (stp-url-read-version (stp-prefix-prompt prompt-prefix "Version: "))))))
         `(,pkg-name
           (method . ,method)
           (remote . ,remote)
           (version . ,version)))))))

(defun stp-repair-default-callback (type pkg-name)
  (let-alist (stp-get-alist pkg-name)
    (cl-flet ((handle-partial-elpa-url (pkg-name)
                (while (or (not (string-match-p rem-strict-url-regexp .remote))
                           (not (url-file-exists-p .remote))
                           (not (stp-valid-remote-p .remote .method)))
                  (setq .remote (stp-read-remote (format "Invalid URL (or host is down): %s" .remote) .remote)))
                (stp-set-attribute pkg-name 'remote .remote)
                (unless .version
                  (let ((prompt (format "[%s] version: " pkg-name)))
                    (cl-ecase .method
                      (elpa (setq .version (stp-elpa-read-version prompt pkg-name .remote)))
                      (url (setq .version (stp-url-read-version prompt))))
                    (stp-set-attribute pkg-name 'version .version))))
              (handle-partial-archive (pkg-name)
                (stp-archive-ensure-loaded)
                (unless .remote
                  (let ((prompt (format "[%s] archive: " pkg-name)))
                    (setq .remote (stp-comp-read-remote prompt (stp-archives pkg-name)))
                    (stp-set-attribute pkg-name 'remote .remote)))))
      (cl-case type
        (ghost-package (yes-or-no-p (format "%s was found in %s but not in the filesystem in %s. Remove it?" pkg-name stp-info-file stp-source-directory)))
        (invalid-git-remote (stp-git-read-remote (format "The remote %s for %s is invalid or temporarily unavailable; enter remote: " .remote pkg-name)))
        (unknown-git-version (stp-git-read-version (format "Unable to determine verion for %s; enter version: " pkg-name)
                                                   .remote
                                                   :extra-versions (list .branch)))
        (unknown-git-update (stp-git-read-update (format "Unable to determine update for %s; enter update: " pkg-name)))
        (unknown-git-branch (stp-git-read-branch (format "Unable to determine branch for %s; enter branch: " pkg-name) .remote))
        (partial-elpa-package (handle-partial-elpa-url pkg-name))
        (partial-archive-package (handle-partial-archive pkg-name))
        (partial-url-package (handle-partial-elpa-url pkg-name))
        (unknown-package (stp-set-alist pkg-name (cdr (stp-read-package :pkg-name pkg-name :prompt-prefix (format "Package info is missing for %s; " pkg-name)))))))))

(defun stp-valid-remote-p (remote &optional method)
  "Check if REMOTE is a valid remote for some method. If METHOD is
specified, ensure that REMOTE is valid for that specific METHOD."
  (if method
      (funcall (map-elt stp-remote-valid-alist method) remote)
    (and (stp-remote-method remote :noerror t) t)))

(defvar stp-repair-allow-abbreviated-hashes nil)

(cl-defun stp-repair-info (&key (quiet t) (pkg-names (stp-filesystem-names)) (callback #'stp-repair-default-callback))
  "Update package info that does not match the versions in the
package subtrees. Note that not all info can be recovered
automatically. However, it is typically possible to recover the
\\='version attribute for the \\='git method and the \\='update
attribute for any method.

If quiet is nil, print status to show progress. If pkg-names is
the list of the packages to repair. By default all packages will
be repaired.

callback should be a function that can be queried to resolve
exceptional situations. Its arguments have the form (type
pkg-name) where type is a symbol indicating the type of exception
and pkg-name is the name of the package for which the problem
occurred."
  (let* ((i 1)
         (n (length pkg-names)))
    (unwind-protect
        (dolist (pkg-name pkg-names)
          (let ((pkg-name (stp-name pkg-name)))
            (let-alist (stp-get-alist pkg-name)
              (unless quiet
                (message (concat (if (> n 1)
                                     (format "(%d/%d) " i n)
                                   "")
                                 "Analyzing %s...")
                         pkg-name))
              (if (f-dir-p pkg-name)
                  (progn
                    (unless .method
                      ;; This means that pkg-name exists in
                      ;; `stp-source-directory' but is not recorded in
                      ;; `stp-package-info'. In other words, the user installed
                      ;; the package manually without using `stp-install'.
                      (when (stp-git-subtree-package-p pkg-name)
                        (message "A manual installation was detected for %s" pkg-name)
                        (setq .method 'git)
                        (stp-set-attribute pkg-name 'method 'git)))
                    (let* ((valid-other-remotes (-filter (-rpartial #'stp-valid-remote-p .method) .other-remotes)))
                      (when valid-other-remotes
                        (stp-set-attribute pkg-name 'other-remotes valid-other-remotes)))
                    (db (version update)
                        (stp-git-subtree-version pkg-name)
                      (cl-case .method
                        (git
                         ;; First make sure that the remote is valid. This has to
                         ;; be done first since `stp-git-subtree-version' needs to
                         ;; know the remote.
                         (unless (stp-git-valid-remote-p .remote)
                           (setq .remote (funcall callback 'invalid-git-remote pkg-name)))
                         (if .remote
                             (stp-set-attribute pkg-name 'remote .remote)
                           (unless quiet
                             (message "Failed to determine the remote of %s" pkg-name)))
                         ;; Use callback to determine the version if it could not
                         ;; be deduced above.
                         (setq version (or version (funcall callback 'unknown-git-version pkg-name)))
                         (if version
                             ;; Only update hashes if they are different. Shorter
                             ;; versions of hashes are acceptable if
                             ;; `stp-repair-allow-abbreviated-hashes' is non-nil.
                             (unless (and stp-repair-allow-abbreviated-hashes
                                          (stp-git-hash= version .version))
                               (stp-set-attribute pkg-name 'version version))
                           (unless quiet
                             (message "Failed to determine the version of %s" pkg-name)))
                         ;; Use callback to determine update if it could not be
                         ;; deduced above.
                         (setq update (or update (funcall callback 'unknown-git-update pkg-name)))
                         (if update
                             (progn
                               (stp-set-attribute pkg-name 'update update)
                               (when (eq update 'unstable)
                                 ;; If the 'update attribute is 'unstable, there
                                 ;; should be a 'branch attribute. If it is
                                 ;; missing, we try to get it from the callback
                                 ;; function. If that doesn't work, we assume that
                                 ;; it should be the master branch.
                                 (setq .branch
                                       (or .branch
                                           (funcall callback 'unknown-git-branch pkg-name)))
                                 (if .branch
                                     (progn
                                       (stp-set-attribute pkg-name 'branch .branch))
                                   (unless quiet
                                     (message "Failed to determine the update mechanism for %s" pkg-name)))))))
                        (elpa
                         (funcall callback 'partial-elpa-package pkg-name))
                        (archive
                         (funcall callback 'partial-archive-package pkg-name))
                        (url
                         (funcall callback 'partial-url-package pkg-name))
                        ;; nil means that we were unable to determine the method.
                        ;; In this case, we obtain the information via callbacks.
                        ((nil)
                         (funcall callback 'unknown-package pkg-name)))
                      ;; Ensure that the package was installed as a subtree.
                      (when (and (not (stp-git-subtree-package-commit pkg-name))
                                 (yes-or-no-p (format "%s was not installed as a git subtree. Uninstall and reinstall? "
                                                      pkg-name)))
                        (stp-reinstall pkg-name version))))
                (when (funcall callback 'ghost-package pkg-name)
                  (stp-delete-alist pkg-name)))
              (cl-incf i)
              ;; Ensure that the package info file is updated even on a keyboard
              ;; quit or other signal.
              (stp-write-info)))))))

(defvar stp-auto-commit t
  "Automatically commit after the package manager makes changes to the
repository. Note that even if this is ommited, some operations (such as subtree
operations) inherently involve commits and this cannot be disabled.")

(defvar stp-auto-push t
  "Automatically push commits to the default target after auto commiting. This
has no effect unless `stp-auto-commit' is non-nil.")

(defvar stp-post-actions-ask nil
  "Ask before perform post actions if `stp-auto-post-actions' is non-nil.")

(defun stp-do-post-actions-p (pkg-name)
  (yes-or-no-p (format "Perform post install/upgrade actions for %s?" pkg-name)))

(defvar stp-auto-post-actions t
  "When non-nil, automatically perform post actions after the
package manager installs or upgrades a package. The value can be
either t or a list containing any of the symbols \\='build,
update-info-directories and \\='update-load-path which specifies
which actions should be performed after a package is installed or
upgraded. The value t indicates that all post actions should be
performed.")

(defvar stp-auto-load t
  "This variable indicates if newly installed or upgraded packages
should be automatically loaded.")

(defun stp-ensure-no-merge-conflicts ()
  (when (stp-git-merge-conflict-p)
    (user-error "Merge conflicts must be resolved before running this command")))

(defun stp-maybe-ensure-clean ()
  (unless (stp-git-clean-or-ask-p)
    (user-error "Aborted: the repository is unclean")))

(defun stp-commit-push-args ()
  (stp-ensure-no-merge-conflicts)
  (let ((args (if current-prefix-arg
                  (list :do-commit (not stp-auto-commit)
                        :do-push (and (not stp-auto-commit) (not stp-auto-push)))
                (list :do-commit stp-auto-commit :do-push stp-auto-push))))
    (when (plist-get args :do-commit)
      (stp-maybe-ensure-clean))
    args))

(defun stp-commit-push-action-args ()
  (append (stp-commit-push-args)
          (list :do-actions (if current-prefix-arg
                                (not stp-auto-post-actions)
                              stp-auto-post-actions))))

(defvar stp-list-version-length 16)

;; `stp-abbreviate-remote-version' is too slow to used in `stp-list-mode' so a
;; faster but less careful variant is used.
(defun stp-list-abbreviate-version (method version)
  (if (and (eq method 'git)
           ;; This is a crude test to determine if version is a git hash and it
           ;; is not completely correct. A hash might have only letters (though
           ;; it is not likely). It is also possible for a branch or tag to
           ;; match this regexp. However, this is much faster than using
           ;; `stp-git-valid-remote-ref-p' to check
           (string-match-p "^[a-f0-9]*$" version)
           ;; (string-match-p "^[a-f]*[0-9][a-f]*[a-f0-9]*$" version)
           )
      (stp-git-abbreviate-hash version)
    (if (> (length version) stp-list-version-length)
        (concat (s-left stp-list-version-length version) stp-ellipsis)
      version)))

(defun stp-list-package-on-line (&optional offset)
  "Return the name of the package on the line OFFSET lines from
point or nil if no package corresponds to that line."
  (stp-refresh-info)
  (when (derived-mode-p 'stp-list-mode)
    (setq offset (or offset 0))
    (let ((line (line-number-at-pos)))
      (save-excursion
        (forward-line offset)
        (when (= (line-number-at-pos) (+ line offset))
          (when-let ((pkg-name (rem-plain-symbol-at-point)))
            (and (not (save-excursion
                        (beginning-of-line)
                        (bobp)))
                 (not (save-excursion
                        (end-of-line)
                        (eobp)))
                 (not (string= pkg-name ""))
                 (member pkg-name (stp-info-names))
                 pkg-name)))))))

(defun stp-list-package-on-previous-line ()
  (stp-list-package-on-line -1))

(defun stp-list-package-on-next-line ()
  (stp-list-package-on-line 1))

(defun stp-list-other-package ()
  (or (stp-list-package-on-previous-line)
      (stp-list-package-on-next-line)))

(defun stp-list-read-package (prompt)
  "In `stp-list-mode', return the package on the current line if there
is one. Otherwise, prompt the user for a package."
  (stp-refresh-info)
  (or (and (derived-mode-p 'stp-list-mode)
           (stp-list-package-on-line))
      (stp-read-existing-name prompt)))

(cl-defun stp-command-args (&key read-pkg-alist actions (line-pkg t))
  "Prepare an argument list for an interactive to `stp-install',
`stp-uninstall', `stp-upgrade', `stp-repair' or
`stp-toggle-update'. The first argument is the name of the
package. If READ-PKG-LIST is non-nil, a package alist will be
read from the user and included as the second positional
argument. ACTIONS determines if the do-actions keyword argument
should be included. When LINE-PKG is non-nil (as it is by
default), any data that would normally be read from the user will
be inferred from the cursor position when `stp-list-mode' is
active."
  (stp-with-package-source-directory
    (plet* ((args (if actions
                      (stp-commit-push-action-args)
                    (stp-commit-push-args)))
            (do-commit (plist-get args :do-commit))
            (`(,pkg-name . ,pkg-alist) (and read-pkg-alist (stp-read-package)))
            (pkg-name (or pkg-name
                          (if line-pkg
                              (stp-list-read-package "Package name: ")
                            (stp-read-name "Package name: ")))))
      (append (list pkg-name)
              (when read-pkg-alist
                (list pkg-alist))
              args))))

(defvar stp-latest-versions-stale-interval (timer-duration "1 day")
  "The number of seconds until the cached latest versions in
`stp-latest-versions-cache' are considered stale.")

(defvar stp-latest-versions-cache nil)

(defvar stp-latest-version-async t
  "This indicates if latest versions should be computed asynchronously.")

(defun stp-update-cached-latest (pkg-name)
  (when stp-latest-versions-cache
    (stp-list-update-latest-version pkg-name :quiet t :async stp-latest-version-async)))

(defun stp-prune-cached-latest-versions (&optional pkg-name)
  (let ((pkg-names (stp-info-names)))
    (if pkg-name
        (setq stp-latest-versions-cache (map-delete stp-latest-versions-cache pkg-name))
      (setq stp-latest-versions-cache (map-filter (lambda (pkg-name _pkg-alist)
                                                    (member pkg-name pkg-names))
                                                  stp-latest-versions-cache)))))

(defun stp-install-command ()
  "Install a package interactively as a git subtree. If
`stp-auto-commit', `stp-auto-push', and `stp-auto-post-actions'
are non-nil, commit, push and perform post actions (see
`stp-auto-post-actions'). With a prefix argument, each of these
is negated relative to the default."
  (interactive)
  (stp-refresh-info)
  ;; `stp-install-command' and `stp-install' are separate functions so that
  ;; `stp-command-args' will be called within the same memoization block (which
  ;; greatly improves efficiency).
  (stp-with-package-source-directory
    (stp-with-memoization
      (apply #'stp-install (stp-command-args :actions t :read-pkg-alist t :line-pkg nil)))))

(cl-defun stp-install (pkg-name pkg-alist &key do-commit do-push do-actions (refresh t))
  "Install a package named pkg-name that has the alist pkg-alist. If
do-commit is non-nil, then automatically commit to the Git
repository after installing the package. If both do-commit and
do-push are non-nil then push to the remote repository as well.
If do-actions is non-nil, `stp-post-actions' will be called after
the package has been installed."
  ;; pkg-name may be nil in interactive calls when the user answers no when the
  ;; repository is dirty and `stp-git-clean-or-ask-p' is called.
  (when pkg-name
    (save-window-excursion
      (let-alist pkg-alist
        ;; Guess the method if it isn't already known.
        (unless .method
          (setq .method (stp-remote-method .remote))
          (stp-set-attribute pkg-name 'method .method))
        (when (stp-url-safe-remote-p .remote)
          (cl-ecase .method
            (git (stp-git-install pkg-name .remote .version .update :branch .branch))
            (elpa (stp-elpa-install pkg-name .remote .version))
            (archive (stp-archive-install pkg-name .remote))
            (url (stp-url-install pkg-name .remote .version)))
          (stp-update-remotes pkg-name .remote .remote .other-remotes)
          (stp-write-info)
          ;; For archives, the version is determined automatically instead of
          ;; being read and so .version will be nil here.
          (setq .version (stp-get-attribute pkg-name 'version))
          (stp-git-commit-push (format "Installed version %s of %s"
                                       (stp-abbreviate-remote-version pkg-name .method .remote .version)
                                       pkg-name)
                               do-commit
                               do-push)
          (when do-actions
            (stp-post-actions pkg-name))
          (when refresh
            (stp-update-cached-latest pkg-name)
            (stp-list-refresh :quiet t)))))))

(defun stp-uninstall-command ()
  "Uninstall a package interactively."
  (interactive)
  (stp-refresh-info)
  (stp-with-package-source-directory
    (stp-with-memoization
      (apply #'stp-uninstall (stp-command-args)))))

(cl-defun stp-uninstall (pkg-name &key do-commit do-push (refresh t))
  "Uninstall the package named pkg-name. The do-commit and do-push arguments are
as in `stp-install'."
  (when pkg-name
    (let-alist (stp-get-alist pkg-name)
      (save-window-excursion
        (if (eql (call-process-shell-command (format "git rm -r '%s'" pkg-name)) 0)
            (progn
              (f-delete pkg-name t)
              (stp-delete-alist pkg-name)
              (stp-write-info)
              (stp-delete-load-path pkg-name)
              (stp-git-commit-push (format "Uninstalled version %s of %s"
                                           (stp-abbreviate-remote-version pkg-name .method .remote .version)
                                           pkg-name)
                                   do-commit
                                   do-push)
              (when refresh
                (stp-list-refresh :quiet t))
              (stp-prune-cached-latest-versions pkg-name))
          (error "Failed to remove %s. This can happen when there are uncommitted changes in the git repository" pkg-name))))))

(defvar stp-git-upgrade-always-offer-remote-heads t)

(defun stp-upgrade-command ()
  "Upgrade a package interactively."
  (interactive)
  (stp-refresh-info)
  (stp-with-package-source-directory
    (stp-with-memoization
      (apply #'stp-upgrade (stp-command-args :actions t)))))

(cl-defun stp-upgrade (pkg-name &key do-commit do-push do-actions (refresh t))
  "Change the version of the package named pkg-name. The do-commit,
do-push and proceed arguments are as in `stp-install'."
  (when pkg-name
    (save-window-excursion
      (let-alist (stp-get-alist pkg-name)
        ;; Automatically determine missing other remotes for archive packages.
        (when (eq .method 'archive)
          (setq .other-remotes (cl-set-difference (stp-archives pkg-name) (cons .remote .other-remotes))))
        (let* ((chosen-remote (stp-choose-remote "Remote: " .remote .other-remotes))
               (extra-versions (and (eq .method 'git)
                                    (or stp-git-upgrade-always-offer-remote-heads
                                        (eq .update 'unstable))
                                    (stp-git-remote-heads-sorted chosen-remote)))
               (prompt (format "Upgrade from %s to version: " (stp-abbreviate-remote-version pkg-name .method chosen-remote .version))))
          (when (stp-url-safe-remote-p chosen-remote)
            (when (and .branch (member .branch extra-versions))
              (setq extra-versions (cons .branch (remove .branch extra-versions))))
            (cl-ecase .method
              (git (--> extra-versions
                        (stp-git-read-version prompt chosen-remote :extra-versions-position (if (eq .update 'unstable) 'first 'last) :extra-versions it :branch-to-hash nil)
                        (stp-git-upgrade pkg-name chosen-remote it)))
              (elpa (->> (stp-elpa-read-version prompt pkg-name chosen-remote)
                         (stp-elpa-upgrade pkg-name chosen-remote)))
              (archive (stp-archive-upgrade pkg-name .remote))
              (url (->> (stp-url-read-version prompt)
                        (stp-url-upgrade pkg-name chosen-remote))))
            ;; The call to `stp-get-attribute' can't be replaced with
            ;; .version because the 'version attribute will have changed
            ;; after the call to `stp-git-upgrade', `stp-elpa-upgrade' or
            ;; `stp-url-upgrade'.
            (let ((new-version (stp-get-attribute pkg-name 'version)))
              (stp-update-remotes pkg-name chosen-remote .remote .other-remotes)
              (stp-write-info)
              ;; Don't commit, push or perform push actions when there are
              ;; merge conflicts.
              (unless (stp-git-merge-conflict-p)
                (stp-git-commit-push (format "Installed version %s of %s"
                                             (stp-abbreviate-remote-version pkg-name .method chosen-remote new-version)
                                             pkg-name)
                                     do-commit
                                     do-push)
                (when do-actions
                  (stp-post-actions pkg-name)))
              (when refresh
                (stp-update-cached-latest pkg-name)
                (stp-list-refresh :quiet t)))))))))

(cl-defun stp-reinstall-command ()
  "Uninstall and reinstall a package interactively as the same version."
  (interactive)
  (stp-refresh-info)
  (stp-with-package-source-directory
    (stp-with-memoization
      (apply #'stp-reinstall (stp-command-args :actions t)))))

(cl-defun stp-reinstall (pkg-name version &key do-commit do-push do-actions refresh skip-subtree-check)
  "Uninstall and reinstall PKG-NAME as VERSION."
  ;; Warn the user about reinstalling if there are modifications to the subtree
  ;; that were not the result of git subtree merge as this will result in the
  ;; loss of their customizations to the package. This is done by checking the
  ;; git history, the worktree and the staged changes for modifications to the
  ;; package.
  (let-alist (stp-get-alist pkg-name)
    (when (and (or (and (not skip-subtree-check)
                        (stp-git-subtree-package-modified-p pkg-name .remote .version))
                   (stp-git-tree-package-modified-p pkg-name))
               (not (yes-or-no-p (format "The package %s has been manually modified. Reinstalling will delete these changes. Do you wish to proceed?" pkg-name))))
      (user-error "Reinstall aborted"))
    (let ((pkg-alist (stp-get-alist pkg-name)))
      ;; Committing is required here because otherwise `git-install' will fail.
      ;; Refreshing the stp-list-buffer-name buffer is suppressed since that will
      ;; be done by stp-upgrade (which calls this command).
      (stp-uninstall pkg-name :do-commit t :refresh nil)
      (setf (map-elt pkg-alist 'version) version)
      ;; The :do-commit argument is not required here. The decisions to
      ;; commit, push or perform post actions will be handled at a
      ;; higher level by `stp-upgrade'.
      (stp-install pkg-name pkg-alist :do-commit do-commit :do-push do-push :do-actions do-actions :refresh refresh))))

(defun stp-repair-command (&optional arg)
  "Repair the stored information for a package interactively. With a
prefix argument, repair all packages."
  (interactive "P")
  (stp-refresh-info)
  (stp-with-package-source-directory
    (stp-with-memoization
      (if arg
          (stp-repair-all-command)
        (apply #'stp-repair (stp-command-args))))))

(cl-defun stp-repair (pkg-name &key do-commit do-push (refresh t))
  "Repair the package named pkg-name. The do-commit, do-push and proceed
arguments are as in `stp-install'."
  (when pkg-name
    (save-window-excursion
      (stp-repair-info :quiet nil :pkg-names (list pkg-name))
      (stp-write-info)
      (stp-git-commit-push (format "Repaired the source package %s" pkg-name) do-commit do-push)
      (when refresh
        (stp-list-refresh :quiet t)))))

(defun stp-repair-all-command ()
  (interactive)
  (stp-refresh-info)
  (stp-with-package-source-directory
    (stp-with-memoization
      (apply #'stp-repair-all (stp-commit-push-args)))))

(cl-defun stp-repair-all (&key do-commit do-push (refresh t))
  "Run `stp-repair-info' and write the repaired package info to
`stp-info-file'."
  (stp-refresh-info)
  (save-window-excursion
    (stp-repair-info :quiet nil)
    (stp-write-info)
    (stp-git-commit-push (format "Repaired source packages") do-commit do-push)
    (when refresh
      (stp-list-refresh :quiet t))))

(defun stp-edit-remotes-command ()
  "Edit the remotes of a package interactively and write the new
package info to `stp-info-file'"
  (interactive)
  (stp-ensure-no-merge-conflicts)
  (stp-refresh-info)
  (stp-with-memoization
    (apply #'stp-edit-remotes (stp-command-args))))

(cl-defun stp-edit-remotes (pkg-name &key do-commit do-push (refresh t))
  "Edit the remote and other-remotes attributes of PKG-NAME using
`completing-read-multiple'. The first chosen will be remotes and
the rest will be other-remotes."
  (let-alist (stp-get-alist pkg-name)
    (if (and pkg-name .remote (not (eq .method 'archive)))
        (let* ((new-remotes (stp-comp-read-remote "Remotes: " (cons .remote .other-remotes) :default .remote :multiple t))
               (new-remote (car new-remotes))
               (new-other-remotes (cdr new-remotes))
               (invalid-remotes (-filter (lambda (remote)
                                           (not (stp-valid-remote-p remote .method)))
                                         new-remotes)))
          (unless new-remotes
            (user-error "At least one remote must be specified"))
          (when invalid-remotes
            (user-error "%s %s not valid for method %s"
                        (if (= (length invalid-remotes) 1) "is" "are")
                        (rem-join-and invalid-remotes) .method))
          (stp-set-attribute pkg-name 'remote new-remote)
          (if new-other-remotes
              (stp-set-attribute pkg-name 'other-remotes new-other-remotes)
            (stp-delete-attribute pkg-name 'other-remotes))
          (stp-write-info)
          (stp-git-commit-push (format "Set remote to %s and other remotes to %S for %s"
                                       new-remote
                                       new-other-remotes
                                       pkg-name)
                               do-commit
                               do-push)
          (when refresh
            (stp-list-refresh :quiet t)))
      (message "There are no remotes to edit%s"
               (if pkg-name
                   (format "for %s" pkg-name)
                 "")))))

(defun stp-toggle-update-command ()
  "Toggle the update attribute of a package interactively."
  (interactive)
  (stp-refresh-info)
  (stp-with-memoization
    (apply #'stp-toggle-update (stp-command-args))))

(cl-defun stp-toggle-update (pkg-name &key do-commit do-push (refresh t))
  "Toggle the update attribute for the package named pkg-name between stable and
unstable."
  (when pkg-name
    (let-alist (stp-get-alist pkg-name)
      (if (eq .method 'git)
          (progn
            (stp-set-attribute pkg-name 'update (stp-invert-update .update))
            (if (eq .update 'stable)
                (progn
                  (setq .branch (or .branch
                                    (stp-git-read-branch "Branch: " .remote)))
                  (stp-set-attribute pkg-name 'branch .branch))
              (stp-delete-attribute pkg-name 'branch))
            (stp-write-info)
            (stp-git-commit-push (format "Changed update to %s for %s"
                                         (stp-invert-update .update)
                                         pkg-name)
                                 do-commit
                                 do-push)
            (when refresh
              (stp-list-refresh :quiet t)))
        (user-error "The update attribute can only be toggled for git packages.")))))

(defun stp-post-actions-command ()
  "Perform actions that are necessary after a package is installed
or upgraded such as building, updating info directories loading
the package and updating the load path."
  (interactive)
  (stp-refresh-info)
  (stp-with-memoization
    (stp-post-actions (stp-list-read-package "Package name: "))))

(defun stp-post-actions (pkg-name)
  (stp-update-load-path (stp-full-path pkg-name))
  (when stp-auto-load
    (stp-reload pkg-name))
  (stp-build pkg-name)
  (stp-build-info pkg-name)
  (stp-update-info-directories pkg-name))

(defvar stp-build-output-buffer-name "*STP Build Output*")

(defvar stp-allow-naive-byte-compile nil
  "If non-nil, packages without a Makefile will be byte-compiled
naively. This might cause problems if the packages need to be
byte-compiled in some special way.")

(defun stp-build-command ()
  "Build a package interactively. When
`stp-allow-naive-byte-compile' is non-nil, byte compilation will
be performed even when no build system is present. The meaning of
`stp-allow-naive-byte-compile' is inverted with a prefix
argument."
  (interactive)
  (stp-refresh-info)
  (stp-with-package-source-directory
    (stp-with-memoization
      (stp-build (stp-list-read-package "Package name: ")
                 (xor stp-allow-naive-byte-compile current-prefix-arg)))))

(defun stp-build (pkg-name &optional allow-naive-byte-compile)
  "If needed, build the package PKG-NAME by running the appropriate
build systems or performing byte compilation. Return non-nil if
there were no errors."
  (save-window-excursion
    (let* ((output-buffer stp-build-output-buffer-name)
           (pkg-path (stp-canonical-path pkg-name))
           (build-dir pkg-path))
      ;; Setup output buffer
      (get-buffer-create output-buffer)
      ;; Handle CMake separately. Since it generates makefiles, make may need
      ;; to be run afterwards.
      (when (f-exists-p (f-expand "CMakeLists.txt" pkg-path))
        (message "CMakeLists.txt was found in %s. Attempting to run cmake..." build-dir)
        ;; Try to use the directory build by default. It is fine if
        ;; this directory already exists as long as it is not tracked
        ;; by git.
        (setq build-dir (f-expand "build" pkg-path))
        (when (and (f-exists-p build-dir)
                   (stp-git-tracked-p build-dir))
          (setq build-dir (f-expand (make-temp-file "build-") pkg-path)))
        (unless (f-exists-p build-dir)
          (make-directory build-dir))
        (let ((default-directory build-dir))
          (let ((cmd "cmake .."))
            (stp-before-build-command cmd output-buffer)
            ;; This will use `build-dir' as the build directory and
            ;; `pkg-path' as the source directory so there is no
            ;; ambiguity as to which CMakeLists.txt file should be
            ;; used.
            (unless (eql (call-process-shell-command cmd nil output-buffer) 0)
              (message "Failed to run cmake on %s" build-dir)))))
      (let ((success
             ;; Try different methods of building the package until one
             ;; succeeds.
             (or nil
                 ;; Handle GNU make. We use a separate binding for
                 ;; `default-directory' here because the cmake code above
                 ;; can change build-dir.
                 (let ((default-directory build-dir))
                   (when (-any (lambda (file)
                                 (f-exists-p file))
                               stp-gnu-makefile-names)
                     (message "A makefile was found in %s. Attempting to run make..." build-dir)
                     (let ((cmd "make"))
                       (stp-before-build-command cmd output-buffer)
                       ;; Make expects a makefile to be in the current directory
                       ;; so there is no ambiguity over which makefile will be
                       ;; used.
                       (or (eql (call-process-shell-command cmd nil output-buffer) 0)
                           (and (message "Failed to run make on %s" pkg-path)
                                nil)))))
                 ;; Note that `byte-recompile-directory' won't recompile files
                 ;; unless they are out of date.
                 (and allow-naive-byte-compile
                      (let ((default-directory pkg-path))
                        (message "Attempting to byte compile files in %s..." pkg-path)
                        (condition-case err
                            (progn
                              ;; Put the messages from `byte-recompile-directory' in
                              ;; output-buffer.
                              (dflet ((message (&rest args)
                                               (with-current-buffer output-buffer
                                                 (insert (apply #'format args)))))
                                (stp-before-build-command "Byte compiling files" output-buffer)
                                (byte-recompile-directory pkg-path 0))
                              t)
                          (error (ignore err)
                                 (message "Byte-compiling %s failed" pkg-path)
                                 nil)))))))
        ;; Return success or failure
        (if success
            (message "Successfully built %s" pkg-name)
          (message "Build failed for %s" pkg-name))
        success))))

(defvar stp-build-blacklist nil
  "This is a list of packages that should not be built by
`stp-build-all' when it is called interactively.")

(defun stp-build-all-command ()
  "Build all packages that need it interactively. When
`stp-allow-naive-byte-compile' is non-nil, byte compilation will
be performed even when no build system is present. The meaning of
`stp-allow-naive-byte-compile' is inverted with a prefix
argument. Packages in `stp-build-blacklist' will not be built."
  (stp-refresh-info)
  (stp-with-package-source-directory
    (stp-with-memoization
      (stp-build-all (cl-set-difference (stp-filesystem-names)
                                        stp-build-blacklist
                                        :test #'equal)
                     (xor stp-allow-naive-byte-compile current-prefix-arg)))))

(defun stp-build-all (&optional pkg-names allow-naive-byte-compile)
  "Build the packages that need it."
  (interactive (list ))
  (let (failed)
    (dolist (pkg-name pkg-names)
      (message "Building %s" pkg-name)
      (unless (stp-build pkg-name allow-naive-byte-compile)
        (push pkg-name failed)))
    (if failed
        (message "Failed to build: %s" (s-join " " failed))
      (message "Successfully built all packages"))))

(defun stp-build-info (pkg-name)
  "Build the info manuals for PKG-NAME."
  (interactive (list (stp-list-read-package "Package name: ")))
  (let* ((makefiles (f-entries (stp-canonical-path pkg-name)
                               (lambda (path)
                                 (member (f-filename path) stp-gnu-makefile-names))
                               t))
         (output-buffer stp-build-output-buffer-name)
         (texi-target (concat pkg-name ".texi"))
         (target (concat pkg-name ".info"))
         attempted
         (success
          ;; Try to build the info manual in different ways until one succeeds.
          (or nil
              ;; Try to find a makefile that has an appropriate target.
              (dolist (makefile makefiles)
                (when (member target (stp-make-targets makefile))
                  (let ((default-directory (f-dirname makefile)))
                    (setq attempted t)
                    (message "Makefile with target %s found in %s. Attempting to run make..." target (f-dirname makefile))
                    (let ((cmd (format "make %s" target)))
                      (stp-before-build-command cmd output-buffer)
                      (if (eql (call-process-shell-command cmd nil output-buffer) 0)
                          (progn
                            (message "Built the info manual for %s using make" pkg-name)
                            (cl-return t))
                        (message "'%s' failed in %s" cmd (f-dirname makefile)))))))

              ;; Try to compile a texi file directly.
              (dolist (source (f-entries (stp-canonical-path pkg-name)
                                         (lambda (path)
                                           (string= (f-filename path) texi-target))
                                         t))
                (let ((default-directory (f-dirname source)))
                  (setq attempted t)
                  (message "texi source file found at %s. Attempting to compile it with makeinfo..." source)
                  (let ((cmd (format "makeinfo --no-split %s" texi-target)))
                    (cond
                     (;; Don't build texi files unless they have changed since the info
                      ;; manual was last built.
                      (f-newer-p (f-swap-ext source "info") source)
                      (message "The info manual for %s is up to date" pkg-name)
                      (cl-return t))
                     ((progn
                        (stp-before-build-command cmd output-buffer)
                        (eql (call-process-shell-command cmd nil output-buffer) 0))
                      (message "Built the info manual for %s using makeinfo" pkg-name)
                      (cl-return t))
                     (t
                      (message "'%s' failed" cmd)))))))))
    (unless attempted
      (message "No makefiles or texi source files found for the %s info manual" pkg-name))
    success))

(defvar stp-build-info-blacklist nil
  "This is a list of packages that should not be built by
`stp-build-all-info' when it is called interactively.")

(defun stp-build-all-info (&optional pkg-names)
  "Build the info manuals for all packages that need it."
  (interactive (list (cl-set-difference (stp-filesystem-names)
                                        stp-build-info-blacklist
                                        :test #'equal)))
  (let (failed)
    (dolist (pkg-name pkg-names)
      (message "Building the info manual for %s" pkg-name)
      (unless (stp-build-info pkg-name)
        (push pkg-name failed)))
    (if failed
        (message "Failed to build info manuals for: %s" (s-join " " failed))
      (message "Successfully built info manuals for all packages"))))

(cl-defun stp-reload (pkg-name &key quiet)
  "Reload the package."
  (interactive (list (stp-list-read-package "Package name: ")))
  ;; Reload the package twice so that macros are handled properly.
  (stp-reload-once pkg-name)
  (stp-reload-once pkg-name)
  (unless quiet
    (message "Reloaded %s" pkg-name)))

(defun stp-update-info-directories (pkg-name &optional quiet)
  "By default, detect info files for all source packages in
`stp-source-directory' and add their directories to
`Info-directory-list'. If PKG-NAME is non-nil, only search for
info files in the directory for that package."
  (interactive (list (stp-list-read-package "Package name: ")))
  (let* ((directory (stp-canonical-path pkg-name))
         (new (mapcar 'f-dirname
                      (f-entries directory
                                 (-partial #'string-match-p "\\.info$")
                                 t))))
    (info-initialize)
    (setq Info-directory-list
          (cl-remove-duplicates (append Info-directory-list new)
                                :test #'equal))
    (unless quiet
      (if new
          (message "Added info files for %s" pkg-name)
        (message "No info files found for %s" pkg-name)))))

(defun stp-update-all-info-directories (&optional pkg-names quiet)
  "Detect info files for all source packages in
`stp-source-directory' and add their directories to
`Info-directory-list'."
  (interactive)
  (setq pkg-names (or pkg-names (stp-filesystem-names)))
  (dolist (pkg-name pkg-names)
    (stp-update-info-directories pkg-name quiet))
  (unless quiet
    (message "Added all info files")))

;; Add info directories for packages that need it.
(with-eval-after-load 'info
  (stp-update-all-info-directories nil t))

(defvar stp-list-error-face 'stp-list-error-face)

(defface stp-list-error-face
  '((t (:inherit error)))
  "Face for packages with errors.")

(defvar stp-list-stale-face 'stp-list-stale-face)

(defface stp-list-stale-face
  '((t (:foreground "DarkOrange")))
  "Face for stale latest versions.")

(defvar stp-list-upgradable-face 'stp-list-upgradable-face)

(defface stp-list-upgradable-face
  '((t (:foreground "blue")))
  "Face for versions that can be upgraded.")

(defvar stp-list-buffer-name "*STP Package List*")

(defvar stp-list-missing-field-string "???")

(defvar stp-list-stale-version-string "?")

(define-derived-mode stp-list-mode special-mode "STP"
  "Major mode for managing source packages. \\{stp-list-mode-map}"
  (visual-line-mode 0)
  ;; Line wrapping isn't appropriate for `stp-list-mode' as it just makes a
  ;; mess.
  (setq-local truncate-lines t))

(defun stp-list-goto-package ()
  "In `stp-list-mode', open the source file that shares the same
name as the package on the current line if such a file exists.
With a prefix argument or if no such file exists, open the
directory for the current package."
  (interactive)
  (let* ((pkg-name (stp-list-read-package "Package name: "))
         (path (stp-main-package-file (stp-canonical-path pkg-name))))
    (find-file path)))

(defun stp-list-open-current-remote (pkg-name)
  "Open the remote for PKG-NAME in the default browser."
  (interactive (list (stp-list-package-on-line)))
  (stp-refresh-info)
  (when pkg-name
    (browse-url (stp-get-attribute pkg-name 'remote))))

(cl-defun stp-list-ensure-package-line ()
  (when (stp-info-names)
    (cond
     ((bobp)
      (forward-line)
      (recenter 1))
     ((eobp)
      (forward-line -1)
      (recenter -1)))
    (beginning-of-line)))

(defun stp-list-scroll-up-command ()
  (interactive)
  (call-interactively #'scroll-up-command)
  (stp-list-ensure-package-line))

(defun stp-list-scroll-down-command ()
  (interactive)
  (call-interactively #'scroll-down-command)
  (stp-list-ensure-package-line))

(defun stp-list-first-package ()
  "Go to the line for the first package."
  (interactive)
  (stp-refresh-info)
  (stp-with-memoization
    (when (stp-info-names)
      (goto-char (point-min))
      (stp-list-ensure-package-line))))

(defun stp-list-last-package ()
  "Go to the line for the last package."
  (interactive)
  (stp-with-memoization
    (when (stp-info-names)
      (goto-char (point-max))
      (stp-list-ensure-package-line))))

(defun stp-list-next-package-with-predicate (predicate &optional n)
  "Go forward to the Nth line from point where predicate is non-nil
and the line corresponds to a package. If the beginning or end of
the buffer is reached before then, go as far forward as possible."
  (setq n (or n 1))
  (let (pt
        valid
        (valid-pt (point))
        (next-line-fun
         (if (>= n 0)
             (lambda ()
               (unless (save-excursion
                         (end-of-line)
                         (eobp))
                 (forward-line)))
           (lambda ()
             (unless (save-excursion
                       (beginning-of-line)
                       (bobp))
               (forward-line -1))))))
    (setq n (abs n))
    (beginning-of-line)
    (while (and (> n 0)
                (not (eql pt (point))))
      (setq pt (point))
      (funcall next-line-fun)
      (while (and (not (setq valid (funcall predicate)))
                  (not (eql pt (point))))
        (setq pt (point))
        (funcall next-line-fun))
      (beginning-of-line)
      (when valid
        (setq valid-pt (point)))
      (cl-decf n))
    (unless valid
      (goto-char valid-pt))))

(defun stp-list-next-package (&optional n)
  "Go to the next package. With a prefix argument, go forward that many packages.
With a negative prefix argument, go backward that many packages."
  (interactive "p")
  (stp-list-next-package-with-predicate #'always n))

(defun stp-list-previous-package (&optional n)
  "Go to the previous package. With a prefix argument, go backward
that many packages. With a negative prefix argument, go forward
that many packages."
  (interactive "p")
  (stp-list-next-package (- n)))

(defun stp-package-upgradable-p (pkg-name)
  ;; Create a combined alist so that latest version information and package
  ;; information can be accessed using `let-alist' since `let-alist' does not
  ;; nest nicely.
  (let-alist (map-merge 'alist
                        (map-elt stp-latest-versions-cache pkg-name)
                        (stp-get-alist pkg-name))
    (stp-version-upgradable-p pkg-name .method .remote .count-to-stable .count-to-unstable .update)))

(defun stp-list-next-upgradable (&optional n)
  "Go to the next package that can be repaired. With a prefix
argument, go forward that many packages. With a negative prefix
argument, go backward that many packages."
  (interactive "p")
  (stp-refresh-info)
  (stp-with-memoization
    (stp-list-next-package-with-predicate (lambda ()
                                            (aand (stp-list-package-on-line)
                                                  (stp-package-upgradable-p it)))
                                          n)))

(defun stp-list-previous-upgradable (&optional n)
  "Go to the previous package that needs to be repaired. With a
prefix argument, go forward that many packages. With a negative
prefix argument, go backward that many packages."
  (interactive "p")
  (stp-list-next-upgradable (- n)))

(defun stp-package-missing-data-p (pkg-name)
  (let-alist (stp-get-alist pkg-name)
    (not (and .method
              .remote
              .version
              (or (not (eq .method 'git))
                  (and .update
                       (or (not (eq .update 'unstable))
                           .branch)))))))

(defun stp-list-next-repair (&optional n)
  "Go to the next package that needs to be repaired. With a prefix
argument, go forward that many packages. With a negative prefix
argument, go backward that many packages."
  (interactive "p")
  (stp-with-memoization
    (stp-list-next-package-with-predicate (lambda ()
                                            (aand (stp-list-package-on-line)
                                                  (stp-package-missing-data-p it)))
                                          n)))

(defun stp-list-previous-repair (&optional n)
  "Go to the previous package that needs to be repaired. With a
prefix argument, go backward that many packages. With a negative
prefix argument, go forward that many packages."
  (interactive "p")
  (stp-list-next-repair (- n)))

(defun stp-latest-version (pkg-name pkg-alist)
  (let-alist pkg-alist
    (let ((timestamp (float-time)))
      (cl-ecase .method
        (git
         (let* ((latest-stable (stp-git-latest-stable-version .remote))
                (latest-unstable (stp-git-latest-unstable-version .remote (or .branch "HEAD")))
                (commits-to-stable (and latest-stable
                                        (stp-git-count-remote-commits .remote .version latest-stable :branch .branch :both t)))
                (commits-to-unstable (and latest-unstable
                                          (stp-git-count-remote-commits .remote .version latest-unstable :branch .branch :both t)))
                (version-timestamp (and .version (stp-git-remote-timestamp .remote .version)))
                (stable-timestamp (and latest-stable (stp-git-remote-timestamp .remote latest-stable)))
                (unstable-timestamp (and latest-unstable (stp-git-remote-timestamp .remote latest-unstable))))
           (when (or latest-stable latest-unstable commits-to-stable commits-to-unstable)
             (append (list pkg-name)
                     (and latest-stable (list `(latest-stable . ,latest-stable)))
                     (and latest-unstable (list `(latest-unstable . ,latest-unstable)))
                     (and commits-to-stable (list `(count-to-stable . ,commits-to-stable)))
                     (and commits-to-unstable (list `(count-to-unstable . ,commits-to-unstable)))
                     (and version-timestamp (list `(version-timestamp . ,version-timestamp)))
                     (and stable-timestamp (list `(stable-timestamp . ,stable-timestamp)))
                     (and unstable-timestamp (list `(unstable-timestamp . ,unstable-timestamp)))
                     (list `(updated . ,timestamp))))))
        (elpa
         (let* ((latest-stable (stp-elpa-latest-version pkg-name .remote))
                (versions-to-stable (and latest-stable (stp-elpa-count-versions pkg-name .remote .version latest-stable))))
           (unless latest-stable
             (error "Failed to get the latest stable version for %s" pkg-name))
           ;; Occasionally, it is possible we may run into a package when
           ;; versions-to-stable is nil because the current version is invalid and
           ;; does not appear in the list of versions on ELPA.
           (append `(,pkg-name
                     (latest-stable . ,latest-stable))
                   (and versions-to-stable (list `(count-to-stable . ,versions-to-stable)))
                   (list `(updated . ,timestamp)))))
        (archive
         (let ((latest-stable (stp-archive-latest-stable-version pkg-name .remote))
               (latest-unstable (stp-archive-latest-unstable-version pkg-name .remote)))
           (append (list pkg-name)
                   (and latest-stable (list `(latest-stable . ,latest-stable)))
                   (and latest-unstable (list `(latest-unstable . ,latest-unstable)))
                   (list `(updated . ,timestamp)))))
        (url
         nil)))))

(defvar stp-latest-num-processes 16
  "The number of processes to use in parallel to compute the latest
versions. This only has an effect when the latest versions are
computed asynchronously. See `stp-latest-version-async'.")

(defvar stp-latest-retries 3
  "Retry computing the latest version for a package up to this many
times if failures occur.")

(cl-defun stp-latest-versions (package-callback final-callback pkg-names &key quiet async (num-processes stp-latest-num-processes) (max-tries stp-latest-retries))
  "Compute the latest versions for the packages in PACKAGES. Once
the latest version becomes available for package, call
PACKAGE-CALLBACK with the latest version alist as the argument.
Once all latest versions are available, call FINAL-CALLBACK with
the alist mapping the names of the packages to their latest
version alists.

The latest versions are computed asynchronously using
NUM-PROCESSES simultaneously. In case an error occurs while
computing the latest version for a package, it will be retried up
to TRIES times."
  (cond
   ((not (featurep 'queue))
    (display-warning 'STP "Updating the latest versions requires the ELPA queue package"))
   ((and async (not (featurep 'async)))
    (display-warning 'STP "Updating the latest versions asynchronously requires the ELPA async package"))
   (t
    (let (latest-versions
          (queue (make-queue))
          (running 0))
      (dolist (pkg-name pkg-names)
        (queue-enqueue queue (list pkg-name 0)))
      (cl-labels
          ((process-latest-version (data)
             (cl-decf running)
             ;; Process the result of the last call to `stp-latest-version' and
             ;; put the package information back into the queue if there was an
             ;; error.
             (when data
               (db (pkg-name tries latest-version-data error-message)
                   data
                 (cond
                  (latest-version-data
                   (push latest-version-data latest-versions)
                   (when package-callback
                     (funcall package-callback latest-version-data)))
                  (error-message
                   (if (>= tries max-tries)
                       (unless quiet
                         (message "Getting the latest version of %s failed %d times: skipping..." pkg-name tries))
                     (cl-incf tries)
                     (unless quiet
                       (message "Getting the latest version of %s failed (%d/%d): %s" pkg-name tries max-tries error-message))
                     (queue-enqueue queue (list pkg-name tries)))))))
             (compute-next-latest-version))
           (compute-next-latest-version ()
             ;; If there are more packages to process in the queue, start fetching
             ;; the latest version for pkg-name. This is done asynchronously if
             ;; async is non-nil.
             (if (queue-empty queue)
                 (when (and final-callback (= running 0))
                   (funcall final-callback latest-versions))
               (db (pkg-name tries)
                   (queue-dequeue queue)
                 (cl-incf running)
                 (if async
                     ;; Binding `async-prompt-for-password' to nil avoids a bug
                     ;; on certain packages (in particular password-store).
                     (let ((async-prompt-for-password nil))
                       (async-start `(lambda ()
                                       ;; Inject the STP variables and the
                                       ;; caller's load path into the
                                       ;; asynchronous process.
                                       ,(async-inject-variables "^stp-")
                                       (setq load-path ',load-path)
                                       (require 'stp)
                                       ;; pkg-alist is read from disk every time
                                       ;; rather than stored in case some other
                                       ;; STP command (such as an upgrade has
                                       ;; modified the package information while
                                       ;; the latest versions were being
                                       ;; updated).
                                       (stp-refresh-info)
                                       (stp-with-memoization
                                         (let (latest-version-data
                                               (pkg-alist (stp-get-alist ,pkg-name)))
                                           (condition-case err
                                               (setq latest-version-data (stp-latest-version ,pkg-name pkg-alist))
                                             (error
                                              (list ,pkg-name ,tries nil (error-message-string err)))
                                             (:success
                                              (list ,pkg-name ,tries latest-version-data nil))))))
                                    #'process-latest-version))
                   (let (latest-version-data
                         (pkg-alist (stp-get-alist pkg-name)))
                     (process-latest-version
                      (condition-case err
                          (stp-with-memoization
                            (setq latest-version-data (stp-latest-version pkg-name pkg-alist)))
                        (error
                         (list pkg-name tries nil (error-message-string err)))
                        (:success
                         (list pkg-name tries latest-version-data nil))))))))))
        (dotimes (_ (if async (min num-processes (length pkg-names)) 1))
          (unless (queue-empty queue)
            (compute-next-latest-version))))))))

(cl-defun stp-list-update-latest-version (pkg-name &key quiet async focus)
  "This is similar to `stp-list-update-latest-versions' but for a
single package."
  (interactive (let ((async (xor current-prefix-arg stp-latest-version-async)))
                 (list (stp-list-package-on-line)
                       :quiet 'packages
                       :async async
                       :focus (not async))))
  (when pkg-name
    (stp-list-update-latest-versions :pkg-names (list pkg-name) :quiet quiet :async async :focus focus :parallel t)))

(defvar stp-list-latest-versions-min-refresh-interval 3
  "This is the minimum number of seconds after which
`stp-list-refresh' will be called by
`stp-list-update-latest-versions'.")

(defvar stp-list-update-latest-versions-running nil)

(cl-defun stp-list-update-latest-versions (&key (pkg-names (stp-stale-packages)) quiet (async stp-latest-version-async) focus parallel)
  "Compute the latest field in `stp-list-mode' so that the user can
see which packages can be upgraded. This is an expensive
operation that may take several minutes if many packages are
installed. This is done synchronously if
`stp-latest-version-async' is nil and otherwise it is done
asynchronously. A universal prefix argument inverts the meaning
of this variable.

By default, only compute the latest field for packages that are
not already in the cache or were last updated more than
`stp-latest-versions-stale-interval' seconds ago. With a negative
prefix argument, recompute the latest versions for all packages.

Multiple instances of this command will not be allowed to run at
the same time unless PARALLEL is non-nil."
  (interactive (let ((async (xor (consp current-prefix-arg) stp-latest-version-async)))
                 (list :pkg-names (if (>= (prefix-numeric-value current-prefix-arg) 0)
                                      (stp-stale-packages)
                                    t)
                       :quiet 'packages
                       :async async
                       :focus (not async))))
  (stp-refresh-info)
  (stp-prune-cached-latest-versions)
  (db (quiet-toplevel quiet-packages)
      (cl-case quiet
        ((nil)
         (list nil nil))
        (toplevel
         (list t nil))
        (packages
         (list nil t))
        (t
         (list t t)))
    (let* (skipped-refresh
           updated-pkgs
           (last-refresh most-negative-fixnum)
           (pkg-names (if (eq pkg-names t)
                          (stp-info-names)
                        pkg-names))
           ;; Ignore URL packages as there is no way to fetch their latest versions.
           (kept-pkg-names (-filter (lambda (pkg-name)
                                      (not (eq (stp-get-attribute pkg-name 'method) 'url)))
                                    pkg-names))
           (num-ignored (- (length pkg-names) (length kept-pkg-names)))
           (ignored-string (if (> num-ignored 0) (format " (%d ignored)" num-ignored) ""))
           (pkg-names kept-pkg-names)
           (plural (not (= (length kept-pkg-names) 1))))
      (when (and (not parallel) pkg-names)
        (if stp-list-update-latest-versions-running
            (user-error "`stp-list-update-latest-versions' is already running")
          (setq stp-list-update-latest-versions-running t)))
      (if pkg-names
          (progn
            (unless quiet-toplevel
              (if plural
                  (message "Updating the latest versions for %d packages%s" (length pkg-names) ignored-string)
                (message "Updating the latest version for %s" (car pkg-names))))
            (stp-latest-versions
             (lambda (latest-version-data)
               (db (pkg-name . version-alist)
                   latest-version-data
                 (push pkg-name updated-pkgs)
                 (setf (map-elt stp-latest-versions-cache pkg-name) version-alist)
                 ;; Don't refresh too often. This prevents the main
                 ;; process from locking up when there are a large
                 ;; number of asynchronous processes.
                 (if (< (- (float-time) last-refresh)
                        stp-list-latest-versions-min-refresh-interval)
                     (setq skipped-refresh pkg-name)
                   (when focus
                     (stp-list-focus-package pkg-name :recenter-arg -1))
                   (stp-list-refresh :quiet t)
                   (setq skipped-refresh nil
                         last-refresh (float-time)))))
             (lambda (_latest-versions)
               (when skipped-refresh
                 (when focus
                   (stp-list-focus-package skipped-refresh :recenter-arg -1))
                 (stp-list-refresh :quiet t))
               (unless parallel
                 (setq stp-list-update-latest-versions-running nil))
               (unless quiet-toplevel
                 (cond
                  (plural
                   (let* ((num-failed (- (length pkg-names) (length updated-pkgs)))
                          (ignored-failed-string (cond
                                                  ((and (= num-ignored 0) (= num-failed 0))
                                                   "")
                                                  ((= num-ignored 0)
                                                   (format " (%d failed)" num-failed))
                                                  ((= num-failed 0)
                                                   ignored-string)
                                                  (t
                                                   (format " (%d ignored; %d failed)" num-ignored num-failed)))))
                     (message "Finished updating the latest versions for %d packages%s" (length updated-pkgs) ignored-failed-string)))
                  (updated-pkgs
                   (message "Updated the latest version for %s" (car pkg-names)))
                  (t
                   (message "Failed to update the latest version for %s" (car pkg-names))))))
             pkg-names
             :quiet quiet-packages
             :async async))
        (unless quiet-toplevel
          (message "No packages need their latest versions updated%s" ignored-string))))))

(rem-set-keys stp-list-mode-map
              "a" #'stp-post-actions-command
              "b" #'stp-build-command
              "B" #'stp-build-all-command
              "m" #'stp-build-info
              "M" #'stp-build-all-info
              "d" #'stp-uninstall
              "e" #'stp-edit-remotes-command
              "g" #'stp-list-refresh
              "G" #'stp-reload
              "i" #'stp-install-command
              "I" #'stp-update-all-info-directories
              "k" #'stp-uninstall-command
              "l" #'stp-update-load-path
              "L" #'stp-update-load-paths
              "O" #'stp-list-open-current-remote
              "n" #'stp-list-next-upgradable
              "p" #'stp-list-previous-upgradable
              "C-n" #'stp-list-next-package
              "C-p" #'stp-list-previous-package
              "M-n" #'stp-list-next-repair
              "M-p" #'stp-list-previous-repair
              "<next>" #'stp-list-scroll-up-command
              "<prior>" #'stp-list-scroll-down-command
              "C-v" #'stp-list-scroll-up-command
              "M-v" #'stp-list-scroll-down-command
              "<home>" #'stp-list-first-package
              "<end>" #'stp-list-last-package
              "M-<" #'stp-list-first-package
              "M->" #'stp-list-last-package
              "r" #'stp-repair-command
              "R" #'stp-reinstall-command
              "t" #'stp-toggle-update-command
              "u" #'stp-upgrade-command
              "v" #'stp-list-update-latest-version
              "V" #'stp-list-update-latest-versions
              "RET" #'stp-find-package)

(defun stp-list-annotated-version (method version count version-timestamp new-timestamp)
  (let* ((count-string (if (and count (/= count 0))
                           (number-to-string count)
                         ""))
         (time-string (if (and version-timestamp new-timestamp)
                          (let ((seconds (- new-timestamp version-timestamp)))
                            (if (/= (round seconds) 0)
                                (stp-short-format-seconds seconds)
                              ""))
                        ""))
         (separator (if (and (not (string= count-string ""))
                             (not (string= time-string "")))
                        ";"
                      ""))
         (combined-string (concat count-string separator time-string)))
    (and version
         (concat (stp-list-abbreviate-version method version)
                 (if (not (string= combined-string ""))
                     (format "(%s)" combined-string)
                   "")))))

(defun stp-latest-stale-p (seconds updated)
  (or (not updated)
      (> (- seconds updated) stp-latest-versions-stale-interval)))

(defun stp-stale-packages (&optional seconds)
  (setq seconds (or seconds (float-time)))
  (--> stp-latest-versions-cache
       (-filter (lambda (latest-version-data)
                  (let-alist (cdr latest-version-data)
                    (not (stp-latest-stale-p seconds .updated))))
                it)
       (mapcar #'car it)
       (cl-set-difference (stp-info-names)
                          it
                          :test #'equal)))

(defun stp-version-upgradable-p (pkg-name method remote count-to-stable count-to-unstable update)
  "Check if the package can be upgraded to a newer version."
  (cl-ecase method
    (git
     (stp-git-version-upgradable-p count-to-stable count-to-unstable update))
    (elpa
     (stp-elpa-version-upgradable-p count-to-stable))
    (archive
     (stp-archive-version-upgradable-p pkg-name remote))
    ;; URL packages are treated as never being upgradable but this isn't
    ;; reliable since they have no version information available.
    (url)))

(defun stp-list-version-field (pkg-name method remote version count-to-stable count-to-unstable update)
  (if version
      (let ((version-string (stp-list-abbreviate-version method version)))
        (if (stp-version-upgradable-p pkg-name method remote count-to-stable count-to-unstable update)
            (propertize version-string 'face stp-list-upgradable-face)
          version-string))
    stp-list-missing-field-string))

(defun stp-list-latest-field (method version-alist seconds)
  (when version-alist
    (let-alist version-alist
      (let* ((stale (stp-latest-stale-p seconds .updated))
             (stale-string (if stale stp-list-stale-version-string ""))
             (stable-version-string (stp-list-annotated-version method .latest-stable .count-to-stable .version-timestamp .stable-timestamp))
             (unstable-version-string (stp-list-annotated-version method .latest-unstable .count-to-unstable .version-timestamp .unstable-timestamp))
             (version-string
              (format "%s%s %s%s"
                      (or stable-version-string "\t")
                      (if stable-version-string stale-string "")
                      (or unstable-version-string "\t")
                      (if unstable-version-string stale-string ""))))
        (when stale
          (setq version-string (propertize version-string 'face stp-list-stale-face)))
        version-string))))

(cl-defun stp-list-refresh (&key (focus-window-line t) quiet full)
  "Refresh the STP list buffer. When FOCUS-WINDOW-LINE is non-nil,
keep point on the same line in the same position in the window
after refreshing. This argument is ignored when the STP list
buffer is not selected in a window. When QUIET is non-nil, do not
print any status messages. When FULL is non-nil (with a prefix
argument interactively), also update the latest versions, delete
stale cached repositories and refresh `package-archive-contents'
asynchronously."
  (interactive (list :full current-prefix-arg))
  (stp-refresh-info)
  (when full
    (unless stp-list-update-latest-versions-running
      (stp-list-update-latest-versions :quiet quiet :async t))
    (stp-git-delete-stale-cached-repos)
    (unless stp-archive-async-refresh-running
      (stp-archive-async-refresh :quiet quiet)))
  (when-let ((buf (get-buffer stp-list-buffer-name)))
    (let ((win (get-buffer-window buf)))
      (with-current-buffer buf
        (let ((column (current-column))
              (seconds (float-time))
              (line (line-number-at-pos))
              (window-line (when (and focus-window-line win)
                             (beginning-of-line)
                             (with-selected-window win
                               (rem-window-line-number-at-pos)))))
          (read-only-mode 0)
          (erase-buffer)
          (insert (format "Package Version%s Method Update Branch Remote\n"
                          (if stp-latest-versions-cache
                              (format " Latest%sstable Latest%sunstable" stp-no-break-space stp-no-break-space)
                            "")))
          (dolist (pkg-name (stp-info-names))
            (let ((version-alist (map-elt stp-latest-versions-cache pkg-name)))
              ;; Nesting `let-alist' doesn't work nicely so we merge the alists
              ;; instead.
              (let-alist (map-merge 'alist version-alist (stp-get-alist pkg-name))
                (insert (format "%s %s %s %s %s %s %s\n"
                                (stp-name pkg-name)
                                (or (stp-list-version-field pkg-name .method .remote .version .count-to-stable .count-to-unstable .update)
                                    (propertize stp-list-missing-field-string 'face stp-list-error-face))
                                (or (when stp-latest-versions-cache
                                      (or (when version-alist
                                            (stp-list-latest-field .method version-alist seconds))
                                          "\t \t"))
                                    "")
                                (if .method
                                    (symbol-name .method)
                                  stp-list-missing-field-string)
                                ;; Instead of using
                                ;; `stp-list-missing-field-string' when the
                                ;; update or branch is missing, simply omit it.
                                (or .update "\t")
                                (or .branch "\t")
                                (or .remote stp-list-missing-field-string))))))
          ;; Align columns. We explicitly use a space so that tab characters will
          ;; count as a column (see how branches are handled above).
          (let ((align-large-region nil))
            (align-regexp (point-min) (point-max) "\\( *\\) +" nil nil t))
          (goto-char (point-min))
          (read-only-mode 1)
          (when (and focus-window-line win)
            (with-selected-window win
              (rem-goto-line line)
              (rem-move-current-window-line-to-pos window-line)
              (beginning-of-line)
              (forward-char column)))
          (unless quiet
            (message "Refreshed packages")))))))

(cl-defun stp-list-focus-package (pkg-name &key (recenter t) (recenter-arg nil))
  (save-match-data
    (goto-char (point-min))
    (re-search-forward (concat "^" (regexp-quote pkg-name)))
    (beginning-of-line)
    (when recenter
      (recenter recenter-arg))))

;; This is disabled by default because it is a bit sluggish and makes the UI
;; hang for a few seconds.
(defvar stp-list-auto-update-latest-versions nil)
(defvar stp-list-auto-delete-stale-cached-repos t)
(defvar stp-list-auto-refresh-package-archives t)

(defun stp-list (&optional arg)
  "List the packages installed in `stp-source-directory'. When
 `stp-list-auto-update-latest-versions',
 `stp-list-auto-delete-stale-cached-repos' and
 `stp-list-auto-refresh-package-archives' are non-nil update the
 latest versions, delete stale cached repositories and refresh
 the package archives asynchronously."
  (interactive "P")
  (stp-refresh-info)
  (let* ((default-directory stp-source-directory)
         (exists (get-buffer stp-list-buffer-name))
         (buf (get-buffer-create stp-list-buffer-name)))
    (pop-to-buffer buf)
    (unless exists
      (stp-list-mode)
      (when (and (featurep 'async)
                 (not stp-list-update-latest-versions-running)
                 (xor stp-list-auto-update-latest-versions arg))
        (stp-list-update-latest-versions :quiet t :async t))
      (when (xor stp-list-auto-delete-stale-cached-repos arg)
        (stp-git-delete-stale-cached-repos))
      (when (and (featurep 'async)
                 (not stp-archive-async-refresh-running)
                 (xor stp-list-auto-refresh-package-archives arg))
        (stp-archive-async-refresh :quiet t))
      (stp-list-refresh :quiet t))))

(cl-defun stp-delete-orphans (&optional (orphan-type 'both) (confirm t))
  "Remove packages that exist in `stp-info-file' but not on the
filesystem. When ORPHAN-TYPE is \\='info, remove entries in
`stp-info-file' that do not have a corresponding package
directory in `stp-source-directory'. When it is \\='filesystem,
delete directories in `stp-source-directory' that do not
correspond to an entry in `stp-source-directory'. When it is
\\='both, remove both types of orphans. When CONFIRM is non-nil,
ask before deleting any entries or directories.

Interactively, both types of orphans are removed and confirmation
is requested by default. With a prefix argument, disable
confirmation."
  (interactive (list 'both (not current-prefix-arg)))
  (stp-refresh-info)
  (let ((deleted-dirs 0)
        (deleted-entries 0)
        (filesystem-pkgs (stp-filesystem-names))
        (info-pkgs (stp-info-names)))
    (when (memq orphan-type '(info both))
      (let ((k 0)
            (orphaned-info-names (cl-set-difference info-pkgs filesystem-pkgs :test #'equal)))
        (unwind-protect
            (dolist (target-name orphaned-info-names)
              (cl-delete-if (lambda (pkg)
                              (let ((name (car pkg)))
                                (and (string= name target-name)
                                     (cl-incf k)
                                     (or (not confirm)
                                         (yes-or-no-p (format "(%d/%d) The directory for %s in %s is missing. Remove the entry in %s?" k (length orphaned-info-names) name stp-source-directory stp-info-file)))
                                     (cl-incf deleted-entries))))
                            stp-package-info))
          ;; Make sure that changes are written to disk each time so that
          ;; progress isn't lost of the user aborts.
          (stp-write-info))))
    (when (memq orphan-type '(filesystem both))
      (let ((k 1)
            (orphaned-dir-names (cl-set-difference filesystem-pkgs info-pkgs :test #'equal)))
        (dolist (dir orphaned-dir-names)
          (when (or (not confirm)
                    (yes-or-no-p (format "(%d/%d) The directory %s in %s has no entry in %s. Delete the directory?" k (length orphaned-dir-names) dir stp-source-directory stp-info-file)))
            (f-delete (stp-canonical-path dir) t)
            (cl-incf deleted-dirs))
          (cl-incf k))))
    (message "Deleted %d orphaned entries in %s and %d orphaned directories in %s"
             deleted-entries
             stp-info-file
             deleted-dirs
             stp-source-directory)))

(defun stp-find-package (pkg-name &optional file arg)
  "Try to find FILE for the package PKG-NAME in the other source
location on the local filesystem.

This is done by looking for a directory named PKG-NAME in a
remote on the local filesystem,
`stp-read-remote-default-directory' or `stp-source-directory'. If
more than one of these exists and does not contain the current
file, the user will be prompted to choose between them. If FILE
is non-nil, open the corresponding file in this directory.
Otherwise (or with a prefix argument), open PKG-NAME.

This command is helpful for switching between the installed
version of package and a local copy of git repository used for
development or for opening packages from `stp-list-mode'."
  (interactive (if (derived-mode-p 'stp-list-mode)
                   (list (stp-list-package-on-line) nil current-prefix-arg)
                 (append (stp-split-current-package) (list current-prefix-arg))))
  (stp-refresh-info)
  (let ((path (f-canonical (or buffer-file-name default-directory))))
    (let-alist (stp-get-alist pkg-name)
      ;; Prefer a remote on the local filesystem or
      ;; `stp-read-remote-default-directory'. If neither of these exists,
      ;; fallback on the copy of the package in `stp-source-directory'.
      (let ((dirs (-filter (lambda (dir)
                             ;; Ignore directories that do not exist and the
                             ;; copy of the package that we are currently in.
                             (and (f-dir-p dir)
                                  (not (f-same-p dir path))
                                  (not (f-ancestor-of-p (f-canonical dir) path))))
                           (append (and .remote (list .remote))
                                   .other-remotes
                                   (list (f-slash (f-join stp-read-remote-default-directory pkg-name))
                                         (stp-full-path pkg-name))))))
        (setq dirs (cl-remove-duplicates dirs :test #'f-same-p))
        (if dirs
            (let ((dir (f-canonical (if (cdr dirs)
                                        (rem-comp-read "Directory: " dirs :require-match t)
                                      (car dirs)))))
              (let (file-found
                    (default-directory dir)
                    (line (line-number-at-pos))
                    (column (current-column))
                    (window-line (rem-window-line-number-at-pos))
                    (old-buf (current-buffer)))
                (find-file (if arg
                               dir
                             (or (setq file-found file) (stp-main-package-file dir))))
                (unless (rem-buffer-same-p old-buf)
                  (message "Files differ. Line and column may not be preserved"))
                ;; Go to the corresponding line in the file if possible.
                (when file-found
                  (rem-goto-line-column line column t)
                  (rem-move-current-window-line-to-pos window-line))))
          (message "%s was not found in the local filesystem" pkg-name))))))

(provide 'stp)
;;; stp.el ends her
