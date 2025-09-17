;;; subtree-package.el --- Manage packages as git subtrees -*- lexical-binding: t; -*-
;; Copyright (C) 2025 David J. Rosenbaum

;; Author: David J. Rosenbaum <djr7c4@gmail.com>
;; Keywords: convenience elisp git tools vc
;; URL: https://github.com/djr7C4/subtree-package
;; Version: 0.9.12
;; Package-Requires: (
;;   (anaphora "1.0.4")
;;   (async "1.9.9")
;;   (dash "2.20.0")
;;   (emacs "29.1")
;;   (f "0.21.0")
;;   (memoize "1.2.0")
;;   (queue "0.2")
;;   (rem "0.8.0")
;;   (s "1.13.0"))

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
(require 'memoize)
(require 'queue nil t)
(require 'stp-bootstrap)
(require 'stp-utils)
(require 'stp-git)
(require 'stp-elpa)
(require 'stp-archive)
(require 'stp-emacsmirror)
(require 'stp-latest)
(require 'stp-url)
(require 'stp-controller)
(require 'stp-headers)
(require 'stp-locked)
(require 'timer)
(require 'url-handlers)

(defvar stp-memoized-functions '(stp-refresh-info stp-git-download-as-synthetic-repo stp-git-ensure-cached-repo stp-git-valid-remote-p stp-git-remote-hash-alist-memoized stp-git-remote-hash-alist stp-git-valid-rev-p stp-git-timestamp stp-git-tree stp-elpa-version-url-alist stp-achive-get-descs))

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
                    (stp-set-attribute pkg-name 'version .version)))
                (unless .requirements
                  (stp-update-requirements pkg-name)))
              (handle-partial-archive (pkg-name)
                (unless .remote
                  (let ((prompt (format "[%s] archive: " pkg-name)))
                    (setq .remote (stp-comp-read-remote prompt (stp-archives pkg-name)))
                    (stp-set-attribute pkg-name 'remote .remote)))
                (unless .requirements
                  (stp-update-requirements pkg-name))))
      (cl-case type
        (requirements (stp-package-requirements pkg-name))
        (ghost-package (yes-or-no-p (format "%s was found in %s but not in the filesystem in %s. Remove it?" pkg-name stp-info-file stp-source-directory)))
        (invalid-git-remote (stp-git-read-remote (format "The remote %s for %s is invalid or temporarily unavailable; enter remote: " .remote pkg-name)))
        (unknown-git-version (stp-git-read-version (format "Unable to determine the version for %s; enter version: " pkg-name)
                                                   .remote
                                                   :extra-versions (list .branch)))
        (unknown-git-update (stp-git-read-update (format "Unable to determine the update for %s; enter update: " pkg-name)))
        (unknown-git-branch (stp-git-read-branch (format "Unable to determine the branch for %s; enter branch: " pkg-name) .remote))
        (partial-elpa-package (handle-partial-elpa-url pkg-name))
        (partial-archive-package (handle-partial-archive pkg-name))
        (partial-url-package (handle-partial-elpa-url pkg-name))
        (unknown-package (stp-set-alist pkg-name (cdr (stp-read-package :pkg-name pkg-name :prompt-prefix (format "Package info is missing for %s; " pkg-name)))))))))

(defvar stp-repair-allow-abbreviated-hashes nil)

(cl-defun stp-repair-info (options &key (quiet t) (pkg-names (stp-filesystem-names)) (callback #'stp-repair-default-callback))
  "Update package info that differs from the installed subtrees.

Note that not all info can be recovered automatically. However,
it is typically possible to recover the \\='version attribute for
the \\='git method and the \\='update attribute for any method.

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
        (cl-dolist (pkg-name pkg-names)
          (let ((pkg-name (stp-name pkg-name)))
            (let-alist (stp-get-alist pkg-name)
              (unless quiet
                (stp-msg (concat (if (> n 1)
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
                        (stp-msg "A manual installation was detected for %s" pkg-name)
                        (setq .method 'git)
                        (stp-set-attribute pkg-name 'method 'git)))
                    (let* ((valid-other-remotes (-filter (-rpartial #'stp-valid-remote-p .method) .other-remotes)))
                      (when valid-other-remotes
                        (stp-set-attribute pkg-name 'other-remotes valid-other-remotes)))
                    (unless .requirements
                      (setq .requirements (funcall callback 'requirements pkg-name))
                      (stp-update-requirements pkg-name .requirements))
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
                             (stp-msg "Failed to determine the remote of %s" pkg-name)))
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
                             (stp-msg "Failed to determine the version of %s" pkg-name)))
                         ;; Use callback to determine update if it could not be
                         ;; deduced above.
                         (setq .update (or .update update (funcall callback 'unknown-git-update pkg-name)))
                         (if .update
                             (progn
                               (stp-set-attribute pkg-name 'update .update)
                               (cond
                                ((eq .update 'unstable)
                                 ;; If the 'update attribute is 'unstable, there
                                 ;; should be a 'branch attribute. If it is
                                 ;; missing, we try to get it from the callback
                                 ;; function.
                                 (setq .branch
                                       (or .branch
                                           (funcall callback 'unknown-git-branch pkg-name)))
                                 (if .branch
                                     (stp-set-attribute pkg-name 'branch .branch)
                                   (unless quiet
                                     (stp-msg "Failed to determine the branch for %s" pkg-name))))
                                ((eq .update 'stable)
                                 ;; When the update attribute is stable there
                                 ;; shouldn't be a branch.
                                 (stp-delete-attribute pkg-name 'branch))))
                           (unless quiet
                             (stp-msg "Failed to determine the update mechanism for %s" pkg-name))))
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
                        (stp-execute-operations
                         (list (stp-reinstall-operation :pkg-name pkg-name
                                                        :new-version version))
                         options))))
                (when (funcall callback 'ghost-package pkg-name)
                  (stp-delete-alist pkg-name)))
              (unless quiet
                (stp-msg "Finished repairing %s" pkg-name))
              (cl-incf i))))
      ;; Ensure that the package info file is updated even on a keyboard quit or
      ;; other signal.
      (stp-write-info))))

(defun stp-ensure-no-merge-conflicts ()
  (when (stp-git-merge-conflict-p)
    (user-error "Merge conflicts must be resolved before running this command")))

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

(defun stp-list-package-on-previous-line ()
  (stp-list-package-on-line -1))

(defun stp-list-package-on-next-line ()
  (stp-list-package-on-line 1))

(defun stp-list-other-package ()
  (or (stp-list-package-on-previous-line)
      (stp-list-package-on-next-line)))

(cl-defun stp-command-options (&key (class 'stp-package-operation-options) (toggle-p (fn current-prefix-arg)))
  (let ((options (make-instance class)))
    (when (stp-maybe-call toggle-p)
      (stp-toggle-options options))
    (stp-validate-options options)
    options))

(cl-defun stp-command-args (&key pkg-name (prompt-prefix "") pkg-version controller read-pkg-alist (existing-pkg t) (line-pkg t) min-version enforce-min-version)
  "Prepare an argument list for an interactive command.

The first argument included in the list is the name of the
package. If PKG-VERSION is non-nil, the \\='VERSION attribute for
the package will be included as the next positional argument. If
READ-PKG-LIST is non-nil, a package alist will be read from the
user and included as an additional positional argument. When
LINE-PKG is non-nil (as it is by default), any data that would
normally be read from the user will be inferred from the cursor
position when `stp-list-mode' is active. MIN-VERSION is the
minimum version that should be selected for this package. If
ENFORCE-MIN-VERSION is non-nil, this requirement is enforced."
  (stp-with-package-source-directory
    (plet* ((`(,pkg-name . ,pkg-alist)
             (or (and read-pkg-alist
                      (if controller
                          (stp-controller-get-package controller
                                                      pkg-name
                                                      prompt-prefix
                                                      min-version
                                                      enforce-min-version)
                        (stp-read-package :pkg-name pkg-name
                                          :prompt-prefix prompt-prefix
                                          :min-version min-version
                                          :enforce-min-version enforce-min-version)))
                 `(,pkg-name)))
            (pkg-name (or pkg-name
                          (cond
                           (line-pkg
                            (stp-list-read-name "Package name: "))
                           (existing-pkg
                            (stp-read-existing-name "Package name: "))
                           (t
                            (stp-read-name "Package name: "))))))
      (append (list pkg-name)
              (when pkg-version
                (list (stp-get-attribute pkg-name 'version)))
              (when read-pkg-alist
                (list pkg-alist))))))

(defvar stp-requirements-toplevel t)

(cl-defun stp-install-command (&key (refresh t))
  "Install a package.

OPTIONS should be an instance of the class
`stp-additive-operation-options'. It controls the behaviors
corresponding to the names of the class slots. Interactively,
these options can be toggled via a menu when the command is run
with a prefix argument.

These behaviors include whether STP should automatically commit,
push, update `stp-lock-file', require the code to be audited and
perform post actions. The variables `stp-auto-commit',
`stp-auto-push', `stp-auto-lock', `stp-audit-changes' and
`stp-auto-post-actions' control the default values.

Finer grained control of post actions is also available via
OPTIONS. In particular, updating the load path, loading code,
building code, building info manuals and updating info
directories can be individually enabled or disabled. The defaults
are set from `stp-auto-update-load-path', `stp-auto-load',
`stp-auto-build', `stp-auto-build-info' and
`stp-auto-update-info-directories'.

If `stp-auto-commit', `stp-auto-push', `stp-auto-lock',
`stp-auto-reset', `stp-audit-changes' and `stp-auto-post-actions'
are non-nil, commit, push update the lock file, reset to the
original commit on errors or failed audits, and perform post
actions (see `stp-auto-post-actions'). With a prefix argument,
each of these can be toggled via an interactive menu before
running the command."
  (interactive)
  ;; `stp-install-command' and `stp-install' are separate functions so that
  ;; `stp-command-args' will be called within the same memoization block (which
  ;; greatly improves efficiency).
  (stp-with-package-source-directory
    (stp-with-memoization
      (stp-refresh-info)
      (stp-archive-ensure-loaded)
      (stp-emacsmirror-ensure-loaded)
      ;; Allow the user to toggle options before reading the package.
      (let* ((options (stp-command-options :class 'stp-install-operation-options))
             (controller (stp-make-controller :options options)))
        (db (pkg-name pkg-alist)
            ;; This needs to be inside `stp-with-memoization' for efficiency
            ;; reasons so it is here instead of in the interactive spec.
            (stp-command-args :controller controller
                              :read-pkg-alist t
                              :existing-pkg nil
                              :line-pkg nil)
          (stp-controller-append-operations controller
                                            (stp-install-operation :pkg-name pkg-name
                                                                   :pkg-alist pkg-alist))
          (stp-execute controller)
          (stp-update-cached-latest pkg-name)
          (when refresh
            (stp-list-refresh :quiet t)))))))

(cl-defun stp-uninstall-command (&key (refresh t))
  "Uninstall a package interactively."
  (interactive)
  (stp-with-package-source-directory
    (stp-with-memoization
      (stp-refresh-info)
      (let ((options (stp-command-options :class 'stp-uninstall-operation-options)))
        (db (pkg-name)
            (stp-command-args)
          (stp-execute-operations (list (stp-uninstall-operation :pkg-name pkg-name))
                                  options)
          (when refresh
            (stp-list-refresh :quiet t)))))))

(cl-defun stp-upgrade-command (&key (refresh t))
  "Upgrade a package interactively."
  (interactive)
  (stp-with-package-source-directory
    (stp-with-memoization
      (stp-refresh-info)
      (let ((options (stp-command-options :class 'stp-upgrade-operation-options)))
        (db (pkg-name)
            (stp-command-args)
          (stp-execute-operations (list (stp-upgrade-operation :pkg-name pkg-name))
                                  options)
          (stp-update-cached-latest pkg-name)
          (when refresh
            (stp-list-refresh :quiet t)))))))

(defun stp-check-requirements ()
  "Check the requirements of all STP packages and report any that
are not satisfied to the user."
  (interactive)
  (stp-headers-update-features)
  (let* ((requirements (->> (stp-get-info-packages)
                            (mapcan (fn (cl-copy-list (map-elt (cdr %) 'requirements))))
                            stp-headers-merge-elisp-requirements))
         (unsatisfied-requirements (-filter (lambda (requirement)
                                              (db (pkg-sym &optional version)
                                                  requirement
                                                (let ((pkg-name (symbol-name pkg-sym)))
                                                  (not (stp-requirement-satisfied-p pkg-name version t)))))
                                            requirements))
         (msgs (mapcar (lambda (requirement)
                         (db (pkg-sym &optional version)
                             requirement
                           (let ((pkg-name (symbol-name pkg-sym)))
                             (format "%s%s: required by %s"
                                     pkg-name
                                     (if version
                                         (format " %s (%s found)"
                                                 version
                                                 (aif (car (map-elt stp-headers-installed-features pkg-sym))
                                                     it
                                                   "not"))
                                       "")
                                     (rem-join-and (stp-required-by pkg-name))))))
                       unsatisfied-requirements)))
    (pop-to-buffer "*STP requirements*")
    (with-current-buffer "*STP requirements*"
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Unsatisfied requirements:\n%s" (s-join "\n" msgs))))
      (read-only-mode 1))))

(cl-defun stp-package-group-command (fun table &key (class nil class-provided-p) (refresh t))
  (stp-refresh-info)
  (let* ((options (apply #'stp-command-options (rem-maybe-kwd-args class class-provided-p)))
         (pkg-names (-> (stp-read-existing-name "Group or package name: "
                                                :table table
                                                :multiple t)
                        stp-expand-groups)))
    (funcall fun pkg-names (clone options :do-commit t :do-push nil :do-lock nil))
    (with-slots (do-push do-lock)
        options
      (stp-git-push :do-push (stp-maybe-call do-push))
      (when (stp-maybe-call do-lock)
        (stp-update-lock-file))
      (when refresh
        (stp-list-refresh :quiet t)))))

(cl-defun stp-install-or-upgrade-package-group-command (&key (refresh t))
  "Install or upgrade the package groups or packages."
  (interactive)
  (stp-with-package-source-directory
    (stp-with-memoization
      (let ((table (completion-table-in-turn (stp-get-info-group-names)
                                             (stp-info-names)
                                             (stp-package-candidate-names))))
        (stp-package-group-command
         (lambda (pkg-names options)
           (stp-execute-operations
            (mapcar (fn (stp-install-or-upgrade-operation :pkg-name %)) pkg-names)
            options)
           (mapc #'stp-update-cached-latest pkg-names)
           (when refresh
             (stp-list-refresh :quiet t)))
         table
         :class 'stp-additive-operation-options)))))

(defun stp-uninstall-package-group-command ()
  "Uninstall the package groups or packages."
  (interactive)
  (stp-with-package-source-directory
    (stp-with-memoization
      (let ((stp-requirements-toplevel nil)
            (table (completion-table-in-turn (stp-get-info-group-names) (stp-info-names))))
        (stp-package-group-command
         (lambda (pkg-names options)
           (stp-execute-operations
            (mapcar (fn (stp-uninstall-operation :pkg-name %)) pkg-names)
            options))
         table
         :class 'stp-uninstall-operation-options)))))

(defvar stp-fork-directory nil
  "The directory to use for forks. When this is nil,
`stp-source-directory' is used.")

(defvar stp-fork-action #'find-file-other-window
  "The action to execute after a fork is created. The function is
called with the local path to the fork.")

(defun stp-fork-command ()
  "Fork the repository for a package."
  (interactive)
  (stp-refresh-info)
  (let ((pkg-name (stp-list-read-name "Package name: ")))
    (let-alist (stp-get-alist pkg-name)
      (unless (eq .method 'git)
        (user-error "Only packages that use the git method can be forked"))
      (let* ((options (stp-command-options :class 'stp-basic-operation-options))
             (dir (or stp-fork-directory stp-development-directory))
             (remote (stp-choose-remote "Remote: " .remote .other-remotes)))
        (stp-fork pkg-name remote dir options)))))

(cl-defun stp-fork (pkg-name remote dir options)
  (with-slots (do-commit do-push)
      options
    (let-alist (stp-get-alist pkg-name)
      (unless (eq .method 'git)
        (error "Only packages that use the git method can be forked"))
      (unless (executable-find "gh")
        (error "gh is required for forking"))
      (let ((default-directory dir))
        (rem-run-command (list "gh" "repo" "fork" "--clone" "--fork-name" pkg-name remote))
        (let* ((default-directory (f-join dir pkg-name))
               (remotes (-uniq (cl-list* (map-elt (stp-git-remotes) "origin")
                                         remote
                                         .remote
                                         .other-remotes))))
          (stp-set-attribute pkg-name 'remote (car remotes))
          (stp-set-attribute pkg-name 'other-remotes (cdr remotes))
          (stp-with-package-source-directory
            (stp-write-info)
            (stp-git-commit-push (format "Added the remote for the fork of %s" pkg-name) :do-commit do-commit :do-push do-push))
          (funcall stp-fork-action default-directory))))))

(cl-defun stp-reinstall-command (&key (refresh t))
  "Uninstall and reinstall a package interactively as the same version."
  (interactive)
  (stp-with-package-source-directory
    (stp-with-memoization
      (stp-refresh-info)
      (let ((options (stp-command-options :class 'stp-reinstall-operation-options)))
        (db (pkg-name version)
            (stp-command-args :pkg-version t)
          (stp-execute-operations
           (list (stp-reinstall-operation :pkg-name pkg-name :new-version version))
           options)
          (stp-update-cached-latest pkg-name)
          (when refresh
            (stp-list-refresh :quiet t)))))))

(defun stp-add-or-edit-package-group-command ()
  "Add or edit a package group for easily upgrading multiple related
packages at the same time."
  (interactive)
  (stp-ensure-no-merge-conflicts)
  (stp-with-package-source-directory
    (stp-with-memoization
      (stp-refresh-info)
      (let* ((options (stp-command-options))
             (group-name (stp-read-group-name "Group: "))
             (pkg-names (stp-get-info-group group-name))
             (table (completion-table-in-turn pkg-names (stp-info-names))))
        (stp-add-or-edit-package-group group-name
                                       (stp-read-existing-name "Package name: "
                                                               :multiple t
                                                               :table table)
                                       options)))))

(cl-defun stp-add-or-edit-package-group (group-name pkg-names options)
  (with-slots (do-commit do-push do-lock)
      options
    (let ((exists-p (stp-get-info-group group-name)))
      (setq pkg-names (-sort #'string< (-uniq pkg-names)))
      (stp-set-info-group group-name pkg-names)
      (stp-write-info)
      (stp-git-commit-push (format "%s the package group %s"
                                   (if exists-p
                                       "Edited"
                                     "Added")
                                   group-name)
                           :do-commit do-commit
                           :do-push do-push)
      (when (stp-maybe-call do-lock)
        (stp-update-lock-file)))))

(defun stp-delete-package-group-command ()
  "Remove a package group."
  (interactive)
  (stp-ensure-no-merge-conflicts)
  (stp-with-memoization
    (stp-refresh-info)
    (let ((options (stp-command-options)))
      (stp-delete-package-group (stp-read-group-name "Group: ") options))))

(cl-defun stp-delete-package-group (group-name options)
  (with-slots (do-commit do-push do-lock)
      options
    (stp-delete-info-group group-name)
    (stp-write-info)
    (stp-git-commit-push (format "Deleted the package group %s" group-name)
                         :do-commit do-commit
                         :do-push do-push)
    (when (stp-maybe-call do-lock)
      (stp-update-lock-file))))

(defun stp-repair-command ()
  "Repair the stored package information.

With a universal prefix argument, allow command options to be
toggled via an interactive menu. If the prefix argument is
negative, repair all packages."
  (interactive)
  (stp-with-package-source-directory
    (stp-with-memoization
      (stp-refresh-info)
      (if (< (prefix-numeric-value current-prefix-arg) 0)
          (stp-repair-all-command :toggle-p (fn (consp current-prefix-arg)))
        (let ((options (stp-command-options :toggle-p (fn (consp current-prefix-arg)))))
          (apply #'stp-repair
                 (rem-at-end (stp-command-args) options)))))))

(cl-defun stp-repair (pkg-name options &key (refresh t))
  "Repair the package named pkg-name.

The DO-COMMIT, DO-PUSH AND DO-LOCK arguments are as in
`stp-install'."
  (when pkg-name
    (with-slots (do-commit do-push do-lock)
        options
      (stp-repair-info (clone options :do-commit nil :do-push nil :do-lock nil)
                       :quiet nil
                       :pkg-names (list pkg-name))
      (stp-write-info)
      (stp-git-commit-push (format "Repaired the source package %s" pkg-name)
                           :do-commit do-commit
                           :do-push do-push)
      (when (stp-maybe-call do-lock pkg-name)
        (stp-update-lock-file))
      (when refresh
        (stp-list-refresh :quiet t)))))

(cl-defun stp-repair-all-command (&key (toggle-p nil toggle-p-provided-p))
  (interactive)
  (stp-with-package-source-directory
    (stp-with-memoization
      (stp-refresh-info)
      (stp-repair-all (apply #'stp-command-options
                             (rem-maybe-kwd-args toggle-p toggle-p-provided-p))))))

(cl-defun stp-repair-all (options &key (refresh t))
  "Repair the stored package information for all packages."
  (with-slots (do-commit do-push do-lock)
      options
    (stp-repair-info (clone options :do-commit nil :do-push nil :do-lock nil)
                     :quiet nil)
    (stp-write-info)
    (stp-git-commit-push (format "Repaired source packages")
                         :do-commit do-commit
                         :do-push do-push)
    (when (stp-maybe-call do-lock)
      (stp-update-lock-file))
    (when refresh
      (stp-list-refresh :quiet t))))

(defun stp-edit-remotes-command ()
  "Edit the stored remotes of a package."
  (interactive)
  (stp-ensure-no-merge-conflicts)
  (stp-with-memoization
    (stp-refresh-info)
    (let ((options (stp-command-options)))
      (apply #'stp-edit-remotes (rem-at-end (stp-command-args) options)))))

(defvar stp-edit-remotes-long-commit-msg nil)

(cl-defun stp-edit-remotes (pkg-name options &key (refresh t))
  "Edit the remote and other-remotes attributes of PKG-NAME.

This uses `completing-read-multiple'. The first chosen will be
remotes and the rest will be other-remotes. The arguments
DO-COMMIT, DO-PUSH, and DO-LOCK are as in `stp-install'."
  (with-slots (do-commit do-push do-lock)
      options
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
                          (rem-join-and invalid-remotes)
                          (if (= (length invalid-remotes) 1) "is" "are")
                          .method))
            (stp-set-attribute pkg-name 'remote new-remote)
            (if new-other-remotes
                (stp-set-attribute pkg-name 'other-remotes new-other-remotes)
              (stp-delete-attribute pkg-name 'other-remotes))
            (stp-write-info)
            (stp-git-commit-push (if stp-edit-remotes-long-commit-msg
                                     (format "Set remote to %s and other remotes to %S for %s"
                                             new-remote
                                             new-other-remotes
                                             pkg-name)
                                   (format "Edited the remotes for %s" pkg-name))
                                 :do-commit do-commit
                                 :do-push do-push)
            (when (stp-maybe-call do-lock pkg-name)
              (stp-update-lock-file))
            (when refresh
              (stp-list-refresh :quiet t)))
        (stp-msg "There are no remotes to edit%s"
                 (if pkg-name
                     (format "for %s" pkg-name)
                   ""))))))

(defun stp-toggle-update-command ()
  "Toggle the update attribute of a package interactively."
  (interactive)
  (stp-with-memoization
    (stp-refresh-info)
    (let ((options (stp-command-options)))
      (apply #'stp-toggle-update (rem-at-end (stp-command-args) options)))))

(cl-defun stp-toggle-update (pkg-name options &key (refresh t))
  "Toggle the update attribute for the package named PKG-NAME.

The arguments DO-COMMIT, DO-PUSH and DO-LOCK are as in
`stp-install'."
  (when pkg-name
    (with-slots (do-commit do-push do-lock)
        options
      (let-alist (stp-get-alist pkg-name)
        (let ((msg (format "Changed update to %s for %s"
                           (stp-invert-update .update)
                           pkg-name)))
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
                (stp-git-commit-push msg
                                     :do-commit do-commit
                                     :do-push do-push)
                (when (stp-maybe-call do-lock pkg-name)
                  (stp-update-lock-file))
                (when refresh
                  (stp-list-refresh :quiet t))
                (stp-msg msg))
            (user-error "The update attribute can only be toggled for git packages.")))))))

(defun stp-toggle-dependency-command ()
  "Toggle the dependency attribute of a package interactively.

This indicates that the packages was installed because another
package requires it rather than explicitly by the user."
  (interactive)
  (let ((options (stp-command-options)))
    (apply #'stp-toggle-dependency (rem-at-end (stp-command-args) options))))

(cl-defun stp-toggle-dependency (pkg-name options)
  (when pkg-name
    (with-slots (do-commit do-push do-lock)
        options
      (let* ((dependency (stp-get-attribute pkg-name 'dependency))
             (msg (format "Changed dependency to %s for %s"
                          (not dependency)
                          pkg-name)))
        (if dependency
            (stp-delete-attribute pkg-name 'dependency)
          (stp-set-attribute pkg-name 'dependency t))
        (stp-write-info)
        (stp-git-commit-push msg
                             :do-commit do-commit
                             :do-push do-push)
        (when (stp-maybe-call do-lock pkg-name)
          (stp-update-lock-file))
        (stp-msg msg)))))

(defun stp-post-actions-command ()
  "Perform actions for newly installed or upgraded packages.

These include building, updating info directories loading the
package and updating the load path."
  (interactive)
  (stp-with-memoization
    (stp-refresh-info)
    (let ((options (stp-command-options :class 'stp-action-operation-options)))
      (stp-post-actions (stp-list-read-name "Package name: ") options))))

(defun stp-lock-file-watcher (event)
  (let ((action (cadr event)))
    (when (memq action '(created changed))
      (stp-checkout-locked-revision))))

(defvar stp-allow-naive-byte-compile nil
  "If non-nil, do not naively byte compile packages.

Naive byte compilation can cause problems if the packages need to
be byte-compiled in some special way.")

(defun stp-build-command ()
  "Build a package interactively.

  When `stp-allow-naive-byte-compile' is non-nil, byte
compilation will be performed even when no build system is
present. The meaning of `stp-allow-naive-byte-compile' is
inverted with a prefix argument."
  (interactive)
  (stp-with-package-source-directory
    (stp-with-memoization
      (stp-refresh-info)
      (stp-build (stp-list-read-name "Package name: ")
                 (xor stp-allow-naive-byte-compile current-prefix-arg)))))

(defvar stp-build-blacklist nil
  "This is a list of packages that should not be built by
  `stp-build-all' when it is called interactively.")

(defun stp-build-all-command ()
  "Build all packages.

When `stp-allow-naive-byte-compile' is non-nil, naive byte
compilation will be performed even when no build system is
present. The meaning of `stp-allow-naive-byte-compile' is
inverted with a prefix argument. Packages in
`stp-build-blacklist' will not be built."
  (interactive)
  (stp-with-package-source-directory
    (stp-with-memoization
      (stp-refresh-info)
      (stp-build-all (cl-set-difference (stp-filesystem-names)
                                        stp-build-blacklist
                                        :test #'equal)
                     (xor stp-allow-naive-byte-compile current-prefix-arg)))))

(defun stp-build-all (&optional pkg-names allow-naive-byte-compile)
  "Build all packages."
  (let (failed)
    (cl-dolist (pkg-name pkg-names)
      (stp-msg "Building %s" pkg-name)
      (unless (stp-build pkg-name allow-naive-byte-compile)
        (push pkg-name failed)))
    (setq failed (reverse failed))
    (if failed
        (stp-msg "Failed to build: %s" (s-join " " failed))
      (stp-msg "Successfully built all packages"))))

(defvar stp-build-info-blacklist nil
  "This is a list of packages that should not be built by
  `stp-build-all-info' when it is called interactively.")

(defun stp-build-all-info (&optional pkg-names)
  "Build the info manuals for all packages."
  (interactive (list (cl-set-difference (stp-filesystem-names)
                                        stp-build-info-blacklist
                                        :test #'equal)))
  (let (failed)
    (cl-dolist (pkg-name pkg-names)
      (stp-msg "Building the info manual for %s" pkg-name)
      (unless (stp-build-info pkg-name)
        (push pkg-name failed)))
    (setq failed (reverse failed))
    (if failed
        (stp-msg "Failed to build info manuals for: %s" (s-join " " failed))
      (stp-msg "Successfully built info manuals for all packages"))))

(cl-defun stp-list-update-load-path (&optional arg)
  "Reload the package."
  (interactive "P")
  (if arg
      (stp-update-load-paths t)
    (stp-update-load-path (stp-canonical-path (stp-list-read-name "Package name: ")) t)))

(defun stp-update-all-info-directories (&optional pkg-names quiet)
  "Make the info files for all packages available to info commands."
  (interactive)
  (setq pkg-names (or pkg-names (stp-filesystem-names)))
  (cl-dolist (pkg-name pkg-names)
    (stp-update-info-directories pkg-name quiet))
  (unless quiet
    (stp-msg "Added all info files")))

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
  "Major mode for managing source packages."
  (visual-line-mode 0)
  ;; Line wrapping isn't appropriate for `stp-list-mode' as it just makes a
  ;; mess.
  (setq-local truncate-lines t))

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
  (stp-with-memoization
    (stp-refresh-info)
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
  "Go forward N lines where predicate is non-nil.

Only lines that correspond to packages are counted. If the
beginning or end of the buffer is reached before then, go as far
forward as possible."
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
  "Go to the next package.

With a prefix argument, go forward that many packages. With a
negative prefix argument, go backward that many packages."
  (interactive "p")
  (stp-list-next-package-with-predicate #'always n))

(defun stp-list-previous-package (&optional n)
  "Go to the previous package.

With a prefix argument, go backward that many packages. With a
negative prefix argument, go forward that many packages."
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
  "Go to the next package that can be repaired.

With a prefix argument, go forward that many packages. With a
negative prefix argument, go backward that many packages."
  (interactive "p")
  (stp-with-memoization
    (stp-refresh-info)
    (stp-list-next-package-with-predicate (lambda ()
                                            (aand (stp-list-package-on-line)
                                                  (stp-package-upgradable-p it)))
                                          n)))

(defun stp-list-previous-upgradable (&optional n)
  "Go to the previous package that needs to be repaired.

With a prefix argument, go forward that many packages. With a
negative prefix argument, go backward that many packages."
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
  "Go to the next package that needs to be repaired.

With a prefix argument, go forward that many packages. With a
negative prefix argument, go backward that many packages."
  (interactive "p")
  (stp-with-memoization
    (stp-list-next-package-with-predicate (lambda ()
                                            (aand (stp-list-package-on-line)
                                                  (stp-package-missing-data-p it)))
                                          n)))

(defun stp-list-previous-repair (&optional n)
  "Go to the previous package that needs to be repaired.

With a prefix argument, go backward that many packages. With a
negative prefix argument, go forward that many packages."
  (interactive "p")
  (stp-list-next-repair (- n)))

(cl-defun stp-list-update-latest-version (pkg-name &key quiet async focus)
  "Update the latest version for PKG-NAME.

This is like `stp-list-update-latest-versions' for a single
package."
  (interactive (let ((async (xor current-prefix-arg stp-latest-version-async)))
                 (list (stp-list-package-on-line)
                       :quiet 'packages
                       :async async
                       :focus (not async))))
  (when pkg-name
    (stp-list-update-latest-versions :pkg-names (list pkg-name) :quiet quiet :async async :focus focus :batch nil)))

(defun stp-update-cached-latest (pkg-name)
  (when stp-latest-versions-cache
    (stp-list-update-latest-version pkg-name :quiet t :async stp-latest-version-async)))

(defvar stp-list-latest-versions-min-refresh-interval 3
  "This is the minimum number of seconds after which
`stp-list-refresh' will be called by
`stp-list-update-latest-versions'.")

(defvar stp-list-update-latest-versions-running nil)

(defvar stp-list-update-latest-versions-batch-polling-interval 0.001)

(cl-defun stp-list-update-latest-versions (&key (pkg-names (stp-stale-packages)) quiet (async stp-latest-version-async) focus (batch t))
  "Compute the latest fields in `stp-list-mode'.

This allows the user to see which packages can be upgraded. This
is an expensive operation that may take several minutes if many
packages are installed. It is performed synchronously if
`stp-latest-version-async' is nil and otherwise it is done
asynchronously. A universal prefix argument inverts the meaning
of this variable.

By default, only compute the latest field for packages that are
not already in the cache or were last updated more than
`stp-latest-versions-stale-interval' seconds ago. With a negative
prefix argument, recompute the latest versions for all packages.

Multiple instances of this command will not be allowed to run at
the same time unless PARALLEL is non-nil.

When BATCH is nil, each time the latest fields become available
for a package, `stp-list-buffer-name' will be updated. This
results in some overhead depending on the number of parallel
processes (see `stp-latest-num-processes') and will make Emacs
less responsive. When BATCH is non-nil, no updates will be
performed until all latest fields have been computed. This will
not slow down Emacs while the fields are being updated."
  (interactive (let ((async (xor (consp current-prefix-arg) stp-latest-version-async)))
                 (list :pkg-names (if (>= (prefix-numeric-value current-prefix-arg) 0)
                                      (stp-stale-packages)
                                    t)
                       :quiet 'packages
                       :async async
                       :focus (not async)
                       :batch t)))
  (unless (featurep 'queue)
    (user-error "Updating the latest versions requires the ELPA queue package"))
  (when (and async (not (featurep 'async)))
    (user-error "Updating the latest versions asynchronously requires the ELPA async package"))
  ;; Synchronous batch updates do not make sense.
  (when (and (not async) batch)
    (error "Synchronous batch updates are not allowed"))
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
      (when (and async (cdr pkg-names))
        (if stp-list-update-latest-versions-running
            (user-error "`stp-list-update-latest-versions' is already running")
          (setq stp-list-update-latest-versions-running t)))
      (if pkg-names
          (cl-flet ((process-package (latest-version-data)
                      (db (pkg-name . version-alist)
                          latest-version-data
                        (push pkg-name updated-pkgs)
                        (setf (map-elt stp-latest-versions-cache pkg-name) version-alist)
                        ;; Don't refresh too often. This prevents the main
                        ;; process from locking up when there are a large number
                        ;; of asynchronous processes.
                        (if (< (- (float-time) last-refresh)
                               stp-list-latest-versions-min-refresh-interval)
                            (setq skipped-refresh pkg-name)
                          (when focus
                            (stp-list-focus-package pkg-name :recenter-arg -1))
                          (stp-list-refresh :quiet t)
                          (setq skipped-refresh nil
                                last-refresh (float-time)))))
                    (process-final (latest-versions)
                      (when batch
                        (setq stp-latest-versions-cache (map-merge 'alist stp-latest-versions-cache latest-versions)))
                      (when (or skipped-refresh batch)
                        (when focus
                          (stp-list-focus-package (or skipped-refresh batch) :recenter-arg -1))
                        (stp-list-refresh :quiet t))
                      (when (and async (cdr pkg-names))
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
                            (stp-msg "Finished updating the latest versions for %d packages%s" (length updated-pkgs) ignored-failed-string)))
                         (updated-pkgs
                          (stp-msg "Updated the latest version for %s" (car pkg-names)))
                         (t
                          (stp-msg "Failed to update the latest version for %s" (car pkg-names)))))))
            (unless quiet-toplevel
              (let ((async-string (if async " asynchronously" "")))
                (if plural
                    (stp-msg "Updating the latest versions for %d packages%s%s" (length pkg-names) async-string ignored-string)
                  (stp-msg "Updating the latest version for %s%s" (car pkg-names) async-string))))
            (if batch
                ;; Create a separate asynchronous process to create the other
                ;; processes. This is much faster than running
                ;; `stp-latest-versions' inside the main Emacs process as the
                ;; calls to `async-start' create a lot of overhead.
                (async-start `(lambda ()
                                ,(async-inject-variables "^stp-" nil (concat stp-async-inject-variables-exclude-regexp stp-async-inject-large-variables-exclude-regexp))
                                (setq load-path ',load-path)
                                (require 'stp)
                                (let (latest-versions
                                      (async ',async)
                                      (pkg-names ',pkg-names)
                                      (quiet-packages ',quiet-packages))
                                  (stp-latest-versions nil
                                                       (lambda (latest-versions2)
                                                         (setq latest-versions (or latest-versions2 t)))
                                                       pkg-names
                                                       :quiet quiet-packages
                                                       :async async)
                                  (while (not latest-versions)
                                    (sleep-for stp-list-update-latest-versions-batch-polling-interval))
                                  (when (eq latest-versions t)
                                    (setq latest-versions nil))
                                  latest-versions))
                             (lambda (latest-versions)
                               (setq updated-pkgs (mapcar #'car latest-versions))
                               (process-final latest-versions)))
              (stp-latest-versions #'process-package
                                   #'process-final
                                   pkg-names
                                   :quiet quiet-packages
                                   :async async)))
        (unless quiet-toplevel
          (stp-msg "No packages need their latest versions updated%s" ignored-string))))))

(rem-set-keys stp-list-mode-map
              "a" #'stp-post-actions-command
              "b" #'stp-build-command
              "B" #'stp-build-all-command
              "c" #'stp-check-requirements
              "m" #'stp-build-info
              "M" #'stp-build-all-info
              "d" #'stp-uninstall-command
              "D" #'stp-uninstall-package-group-command
              "e" #'stp-edit-remotes-command
              "E" #'stp-add-or-edit-package-group-command
              "f" #'stp-fork-command
              "g" #'stp-list-refresh
              "G" #'stp-reload
              "i" #'stp-install-command
              "I" #'stp-update-all-info-directories
              "l" #'stp-list-update-load-path
              "L" #'stp-update-lock-file
              "o" #'stp-unnecessary-dependencies-command
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
              "x" #'stp-delete-package-group-command
              "t" #'stp-toggle-update-command
              "T" #'stp-toggle-dependency-command
              "u" #'stp-upgrade-command
              "U" #'stp-install-or-upgrade-package-group-command
              "v" #'stp-list-update-latest-version
              "V" #'stp-list-update-latest-versions
              "RET" #'stp-find-package)

(defun stp-list-annotated-latest-version (method version count version-timestamp latest-timestamp)
  (and version
       (concat (stp-list-abbreviate-version method version)
               (stp-latest-version-annotation count version-timestamp latest-timestamp))))

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

(defvar stp-list-prefer-latest-stable t
  "When non-nil, if the current version is equivalent to the latest
stable show that instead of the version. This can be useful when
the hash of the latest stable version is stored in the package
database since the latest stable version is always stored as a
tag.")

(defun stp-list-version-field (pkg-name pkg-alist version-alist)
  (let-alist (map-merge 'alist pkg-alist version-alist)
    (when (and stp-list-prefer-latest-stable (equal .count-to-stable 0))
      (setq .version .latest-stable))
    (if .version
        (let ((version-string (stp-list-abbreviate-version .method .version)))
          (setq version-string (if (stp-version-upgradable-p pkg-name .method .remote .count-to-stable .count-to-unstable .update)
                                   (propertize version-string 'face stp-list-upgradable-face)
                                 version-string))
          (when (eq stp-annotated-version-type 'timestamp)
            (setq version-string (format "%s(%s)" version-string (stp-short-format-date .version-timestamp))))
          version-string)
      stp-list-missing-field-string)))

(defun stp-list-latest-field (method version-alist seconds)
  (when version-alist
    (let-alist version-alist
      (let* ((stale (stp-latest-stale-p seconds .updated))
             (stale-string (if stale stp-list-stale-version-string ""))
             (stable-version-string (stp-list-annotated-latest-version method .latest-stable .count-to-stable .version-timestamp .stable-timestamp))
             (unstable-version-string (stp-list-annotated-latest-version method .latest-unstable .count-to-unstable .version-timestamp .unstable-timestamp))
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
  "Refresh the STP list buffer.

When FOCUS-WINDOW-LINE is non-nil, keep point on the same line in
the same position in the window after refreshing. This argument
is ignored when the STP list buffer is not selected in a window.
When QUIET is non-nil, do not print any status messages. When
FULL is non-nil (with a prefix argument interactively), also
update the latest versions, delete stale cached repositories and
refresh `package-archive-contents' and `stp-emacsmirror-alist'
asynchronously."
  (interactive (list :full current-prefix-arg))
  (stp-refresh-info)
  (when full
    (unless stp-list-update-latest-versions-running
      (stp-list-update-latest-versions :quiet quiet :async t))
    (stp-git-delete-stale-cached-repos)
    (unless stp-archive-async-refresh-running
      (stp-archive-async-refresh :quiet quiet))
    (unless stp-emacsmirror-async-refresh-running
      (stp-emacsmirror-async-refresh :quiet quiet)))
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
          (cl-dolist (pkg-name (stp-info-names))
            (let ((pkg-alist (stp-get-alist pkg-name))
                  (version-alist (map-elt stp-latest-versions-cache pkg-name)))
              ;; Nesting `let-alist' doesn't work nicely so we merge the alists
              ;; instead.
              (let-alist (map-merge 'alist pkg-alist version-alist)
                (insert (format "%s %s %s %s %s %s %s\n"
                                (stp-name pkg-name)
                                (or (stp-list-version-field pkg-name pkg-alist version-alist)
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
            (stp-msg "Refreshed packages")))))))

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
(defvar stp-list-auto-refresh-emacsmirror t)

(defun stp-list (&optional arg)
  "List the packages installed in `stp-source-directory'.

When `stp-list-auto-update-latest-versions',
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
      (when (and (featurep 'async)
                 (not stp-archive-async-refresh-running)
                 (xor stp-list-auto-refresh-emacsmirror arg))
        (stp-emacsmirror-async-refresh :quiet t))
      (stp-list-refresh :quiet t))))

(cl-defun stp-delete-ghosts (&optional (ghost-type 'both) (confirm t))
  "Remove packages that do not exist on the filesystem.

These are packages that exist only in `stp-info-file'. When
GHOST-TYPE is \\='info, remove entries in `stp-info-file' that
do not have a corresponding package directory in
`stp-source-directory'. When it is \\='filesystem, delete
directories in `stp-source-directory' that do not correspond to
an entry in `stp-source-directory'. When it is \\='both, remove
both types of orphans. When CONFIRM is non-nil, ask before
deleting any entries or directories.

Interactively, both types of orphans are removed and confirmation
is requested by default. With a prefix argument, disable
confirmation."
  (interactive (list 'both (not current-prefix-arg)))
  (stp-refresh-info)
  (let ((deleted-dirs 0)
        (deleted-entries 0)
        (filesystem-pkgs (stp-filesystem-names))
        (info-pkgs (stp-info-names)))
    (when (memq ghost-type '(info both))
      (let ((k 0)
            (orphaned-info-names (cl-set-difference info-pkgs filesystem-pkgs :test #'equal)))
        (unwind-protect
            (cl-dolist (target-name orphaned-info-names)
              (stp-set-info-packages
               (cl-delete-if (lambda (pkg)
                               (let ((name (car pkg)))
                                 (and (string= name target-name)
                                      (cl-incf k)
                                      (or (not confirm)
                                          (yes-or-no-p (format "(%d/%d) The directory for %s in %s is missing. Remove the entry in %s?" k (length orphaned-info-names) name stp-source-directory stp-info-file)))
                                      (cl-incf deleted-entries))))
                             (stp-get-info-packages))))
          ;; Make sure that changes are written to disk each time so that
          ;; progress isn't lost of the user aborts.
          (stp-write-info))))
    (when (memq ghost-type '(filesystem both))
      (let ((k 1)
            (orphaned-dir-names (cl-set-difference filesystem-pkgs info-pkgs :test #'equal)))
        (cl-dolist (dir orphaned-dir-names)
          (when (or (not confirm)
                    (yes-or-no-p (format "(%d/%d) The directory %s in %s has no entry in %s. Delete the directory?" k (length orphaned-dir-names) dir stp-source-directory stp-info-file)))
            (f-delete (stp-canonical-path dir) t)
            (cl-incf deleted-dirs))
          (cl-incf k))))
    (stp-msg "Deleted %d orphaned entries in %s and %d orphaned directories in %s"
             deleted-entries
             stp-info-file
             deleted-dirs
             stp-source-directory)))

(defun stp-find-package (pkg-name &optional file arg)
  "Try to find FILE for PKG-NAME in the other local source location.

This is done by looking for a directory named PKG-NAME in a
remote on the local filesystem, `stp-development-directory' or
`stp-source-directory'. If more than one of these exists and does
not contain the current file, the user will be prompted to choose
between them. If FILE is non-nil, open the corresponding file in
this directory. Otherwise (or with a prefix argument), open
PKG-NAME.

This command is helpful for switching between the installed
version of package and a local copy of git repository used for
development or for opening packages from `stp-list-mode'."
  (interactive (if (derived-mode-p 'stp-list-mode)
                   (list (stp-list-package-on-line) nil current-prefix-arg)
                 (append (stp-split-current-package) (list current-prefix-arg))))
  (stp-refresh-info)
  (let ((path (f-canonical (or buffer-file-name default-directory))))
    (let-alist (stp-get-alist pkg-name)
      ;; Prefer a remote on the local filesystem or `stp-development-directory'.
      ;; If neither of these exists, fallback on the copy of the package in
      ;; `stp-source-directory'.
      (let ((dirs (-filter (lambda (dir)
                             ;; Ignore directories that do not exist and the
                             ;; copy of the package that we are currently in.
                             (and (f-dir-p dir)
                                  (not (f-same-p dir path))
                                  (not (f-ancestor-of-p (f-canonical dir) path))))
                           (append (and .remote (list .remote))
                                   .other-remotes
                                   (and stp-development-directory
                                        (list (f-slash (f-join stp-development-directory pkg-name))
                                              (stp-full-path pkg-name)))))))
        (setq dirs (cl-remove-duplicates dirs :test #'f-same-p))
        (if dirs
            (let ((dir (f-full (if (cdr dirs)
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
                             (or (setq file-found file) (stp-main-package-file pkg-name :relative t))))
                (when (and (not (with-current-buffer old-buf
                                  (derived-mode-p 'stp-list-mode)))
                           (not (rem-buffer-same-p old-buf)))
                  (stp-msg "Files differ. Line and column may not be preserved"))
                ;; Go to the corresponding line in the file if possible.
                (when file-found
                  (rem-goto-line-column line column t)
                  (rem-move-current-window-line-to-pos window-line))))
          (stp-msg "%s was not found in the local filesystem" pkg-name))))))

(defun stp-unnecessary-dependencies-command (&optional arg)
  "By default, inform the user about dependencies that are no longer
required. With a prefix argument, delete them instead."
  (interactive "P")
  (if arg
      (call-interactively #'stp-delete-unnecessary-dependencies)
    (call-interactively #'stp-show-unnecessary-dependencies)))

(defun stp-show-unnecessary-dependencies ()
  "Print the list of packages that were installed as dependencies
but are no longer required by any other package."
  (interactive)
  (stp-refresh-info)
  (let ((pkgs (stp-find-unnecessary-dependencies)))
    (if pkgs
        (stp-msg " are no longer required and can be uninstalled" (apply #'rem-join-and pkgs))
      (stp-msg "No unnecessary dependencies were found"))))

(defun stp-delete-unnecessary-dependencies (options)
  "Uninstall packages that were installed as dependencies but are no
longer required by any other package."
  (interactive (list (stp-command-options :class 'stp-uninstall-operation-options)))
  (stp-refresh-info)
  (stp-with-package-source-directory
    (stp-with-memoization
      (stp-execute-operations
       (mapcar (fn (stp-uninstall-operation :pkg-name %))
               (stp-find-unnecessary-dependencies))
       options))))

(cl-defun stp-bump-version (filename options)
  "Increase the version header for FILENAME. Interactively, this is
the file for the current buffer or the main file for the package
if no version header is found for the current file."
  (interactive (let ((options (stp-command-options :class 'stp-bump-operation-options)))
                 (list (cl-flet ((has-version-header-p (filename)
                                   (when filename
                                     (when (functionp filename)
                                       (setq filename (funcall filename)))
                                     (with-temp-buffer
                                       (insert-file-contents filename)
                                       (and (stp-headers-version) filename)))))
                         (cl-some #'has-version-header-p
                                  (list (buffer-file-name (buffer-base-buffer))
                                        ;; `stp-main-package-file' can prompt the
                                        ;; user so we don't want to actually call
                                        ;; it unless it's really necessary.
                                        (fn (aand (stp-git-root)
                                                  (stp-main-package-file it)))
                                        (fn (user-error "No Version header was found")))))
                       options)))
  (with-slots (do-commit do-push do-tag)
      options
    (let ((clean (stp-git-clean-p)))
      (save-excursion
        (find-file filename)
        (let* ((version (stp-headers-version))
               (new-version (rem-read-from-mini (format "New version (> %s): " version) :initial-contents version)))
          (unless version
            (error "No Version header was found in this buffer"))
          (unless (and (ignore-errors (version-to-list new-version))
                       (version< version new-version))
            (user-error "%s must a valid version newer than %s" new-version version))
          (delete-region (point) (line-end-position))
          (insert new-version)
          (when (stp-maybe-call do-commit)
            (save-buffer)
            (stp-git-add default-directory :update t)
            (let ((msg (format "Bumped the version to %s" new-version)))
              ;; Give the user a chance to use their own message if we aren't
              ;; just bumping the version in this commit.
              (unless clean
                (setq msg (rem-read-from-mini "Commit message: " :initial-contents msg)))
              (stp-git-commit msg :do-commit t))
            (when (stp-maybe-call do-tag)
              (let ((tag (concat "v" new-version)))
                (stp-git-tag tag (stp-git-head default-directory))
                (stp-msg "Added the git tag %s for %s" tag (stp-git-root :transform #'f-full))))
            (stp-git-push :do-push (stp-maybe-call do-push) :tags t)))))))

(defun stp-savehist-setup ()
  (with-eval-after-load "savehist"
    (defvar savehist-additional-variables)
    (cl-dolist (var '(stp-latest-versions-cache
                   stp-archive-last-refreshed
                   stp-headers-elisp-file-requirements-cache
                   stp-headers-elisp-file-feature-cache
                   stp-headers-installed-features
                   stp-headers-versions))
      (add-to-list 'savehist-additional-variables var))))

(defun stp-setup ()
  (stp-headers-update-features)
  (stp-archive-ensure-loaded)
  (stp-emacsmirror-ensure-loaded)
  (stp-savehist-setup))

(provide 'subtree-package)

;; Local Variables:
;; eval: (add-hook 'stp-headers-update-hook #'stp-headers-write-bootstrap-requirements nil t)
;; read-symbol-shorthands: (
;;   ("db" . "cl-destructuring-bind")
;;   ("mvb" . "cl-multiple-value-bind")
;;   ("mvs" . "cl-multiple-value-setq")
;;   ("with-gensyms" . "cl-with-gensyms")
;;   ("once-only" . "cl-once-only")
;;   ("dflet" . "noflet")
;;   ("plet" . "pcase-let")
;;   ("plet*" . "pcase-let*")
;;   ("psetq*" . "pcase-setq")
;;   ("pdolist" . "pcase-dolist")
;;   ("plambda" . "pcase-lambda")
;;   ("pdefmacro" . "pcase-defmacro")
;;   ("epcase" . "pcase-exhaustive")
;;   ("fn" . "rem-fn")
;;   ("fn1" . "rem-fn1")
;;   ("fn2" . "rem-fn2")
;;   ("fn3" . "rem-fn3")
;;   ("fn4" . "rem-fn4")
;;   ("fn5" . "rem-fn5")
;;   ("fn6" . "rem-fn6")
;;   ("fn7" . "rem-fn7")
;;   ("fn8" . "rem-fn8")
;;   ("fn9" . "rem-fn9")
;;   ("fn10" . "rem-fn10")
;;   ("and$" . "cond-let--and$")
;;   ("and>" . "cond-let--and>")
;;   ("and-let" . "cond-let--and-let")
;;   ("if-let" . "cond-let--if-let")
;;   ("when-let" . "cond-let--when-let")
;;   ("while-let" . "cond-let--while-let"))
;; End:
;;; subtree-package.el ends here
