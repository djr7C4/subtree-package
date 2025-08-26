;;; subtree-package.el --- Manage packages as git subtrees -*- lexical-binding: t; -*-
;; Copyright (C) 2025 David J. Rosenbaum

;; Author: David J. Rosenbaum <djr7c4@gmail.com>
;; Keywords: convenience elisp git tools vc
;; URL: https://github.com/djr7C4/subtree-package
;; Version: 0.9.8
;; Package-Requires: (
;;   (anaphora "1.0.4")
;;   (async "1.9.9")
;;   (dash "2.20.0")
;;   (emacs "29.1")
;;   (f "0.21.0")
;;   (llama "1.0.0")
;;   (memoize "1.2.0")
;;   (queue "0.2")
;;   (rem "0.7.9")
;;   (s "1.12.0"))

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
(require 'stp-archive)
(require 'stp-bootstrap)
(require 'stp-utils)
(require 'stp-elpa)
(require 'stp-git)
(require 'stp-locked)
(require 'stp-headers)
(require 'stp-url)
(require 'timer)
(require 'url-handlers)

(defvar stp-memoized-functions '(stp-refresh-info stp-git-download-as-synthetic-repo stp-git-ensure-cached-repo stp-git-valid-remote-p stp-git-remote-hash-alist-basic stp-git-remote-hash-alist stp-git-valid-rev-p stp-git-timestamp stp-git-tree stp-elpa-version-url-alist stp-achive-get-descs))

(defvar stp-memoization-active nil)

(defmacro stp-with-memoization (&rest body)
  "Evaluate BODY with memoization active for expensive functions.

Cached results are only retained while within the scope of this
macro. This allows functions that would otherwise make many
duplicate queries to remote git repositories to only make one of
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

(defvar stp-current-package nil
  "The name of the package that is currently being operated on.")

(defvar stp-normalize-versions nil
  "Indicates if versions should be printed in a standardized format.

This overrides the specific format used for versions by the
project.")

(defun stp-abbreviate-remote-version (pkg-name method remote version)
  "Abbreviate long hashes to make them more readable.

Other versions are not abbreviated."
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
  (let* ((archive-names (if pkg-name
                            (ensure-list (cl-find pkg-name (stp-archive-package-names) :test #'string=))
                          (stp-archive-package-names)))
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
                 (remotes (append (stp-archive-find-remotes pkg-name)
                                  (mapcar (fn (cons % 'elpa))
                                          (stp-elpa-package-urls pkg-name archives :annotate t))))
                 (remote-or-archive (stp-comp-read-remote
                                     "Remote or archive: "
                                     (->> (append remotes archive-alist)
                                          stp-sort-remotes
                                          (mapcar #'car))
                                     :default (car remotes))))
            (cons pkg-name (or (map-elt archive-alist remote-or-archive)
                               (car (s-split " " remote-or-archive))))))
      ;; Otherwise the user chose a remote so prompt for its package name.
      (let ((remote (stp-normalize-remote name-or-remote)))
        (cons (or pkg-name (stp-read-name (stp-prefix-prompt prompt-prefix "Package name: ") :default (stp-default-name remote)))
              remote)))))

(cl-defun stp-read-package (&key pkg-name pkg-alist (prompt-prefix "") min-version enforce-min-version)
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
           (setq update (stp-git-read-update (stp-prefix-prompt prompt-prefix "Update policy: ")
                                             :default (map-elt pkg-alist 'update)
                                             :remote remote
                                             :other-remotes (map-elt pkg-alist 'other-remotes))))
         (when (and (eq update 'unstable)
                    (not branch))
           (setq branch (stp-git-read-branch (stp-prefix-prompt prompt-prefix "Branch: ") remote (map-elt pkg-alist 'branch))))
         (unless version
           (setq version (stp-git-read-version
                          (stp-prefix-prompt prompt-prefix (format "Version%s: " (stp-min-version-annotation min-version enforce-min-version)))
                          remote
                          :extra-versions (list (map-elt pkg-alist 'version) branch)
                          :default (map-elt pkg-alist 'version)
                          :min-version (and enforce-min-version min-version))))
         `(,pkg-name
           (method . ,method)
           (remote . ,remote)
           (version . ,version)
           (update . ,update)
           (branch . ,branch)))
        ;; Archives only have one version so the minimum version cannot be
        ;; enforced.
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
             (elpa (setq version (stp-elpa-read-version
                                  (stp-prefix-prompt prompt-prefix "Version: ")
                                  pkg-name
                                  remote

                                  :min-version (and enforce-min-version min-version))))
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
                    (stp-set-attribute pkg-name 'version .version)))
                (unless .requirements
                  (stp-update-requirements pkg-name)))
              (handle-partial-archive (pkg-name)
                (stp-archive-ensure-loaded)
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

(defun stp-valid-remote-p (remote &optional method)
  "Check if REMOTE is a valid remote for some method.

If METHOD is specified, ensure that REMOTE is valid for that
specific METHOD."
  (if method
      (funcall (map-elt stp-remote-valid-alist method) remote)
    (and (stp-remote-method remote :noerror t) t)))

(defvar stp-repair-allow-abbreviated-hashes nil)

(cl-defun stp-repair-info (&key (quiet t) (pkg-names (stp-filesystem-names)) (callback #'stp-repair-default-callback))
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
            (setq stp-current-package pkg-name)
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
                               (when (eq .update 'unstable)
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
                                     (stp-msg "Failed to determine the update mechanism for %s" pkg-name)))))))
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
              (unless quiet
                (stp-msg "Finished repairing %s" pkg-name))
              (cl-incf i))))
      ;; Ensure that the package info file is updated even on a keyboard quit or
      ;; other signal.
      (stp-write-info))))

(defvar stp-auto-commit t
  "When non-nil, automatically commit changes.

Note that even if this is ommited, some operations (such as
subtree operations) inherently involve commits and this cannot be
disabled. When this variable is a function it will be called to
determine the value when it is needed.")

(defvar stp-auto-push t
  "When non-nil, automatically push commits.

This has no effect unless `stp-auto-commit'
is non-nil. When this variable is a function it will be called to
determine the value when it is needed.")

(defvar stp-auto-post-actions t
  "When non-nil, automatically perform post actions.

The value can be either t or a list containing any of the symbols
\\='build, update-info-directories and \\='update-load-path which
specifies which actions should be performed after a package is
installed or upgraded. The value t indicates that all post
actions should be performed. When this variable is a function it
will be called to determine the value when it is needed.")

(defvar stp-audit-changes nil
  "Show diffs whenever a package changes or new code is added. This
is useful for security purposes.")

(defvar stp-auto-tag t
  "When bumping the version, automatically tag the commit with the
new version.")

(defvar stp-auto-update-load-path t
  "When non-nil, automatically update the load path.

When this variable is a function it will be called to determine
the value when it is needed.")

(defvar stp-auto-load t
  "When non-nil, automatically load packages.

When this variable is a function it will be called to determine
the value when it is needed.")

(defvar stp-auto-build nil
  "When non-nil, automatically build pacakges.

General methods which may fail for some packages are used. When
this variable is a function it will be called to determine the
value when it is needed.")

(defvar stp-auto-build-info t
  "When non-nil, automatically build info manuals.

When this variable is a function
it will be called to determine the value when it is needed. When
this variable is a function it will be called to determine the
value when it is needed.")

(defvar stp-auto-update-info-directories t
  "When non-nil, automatically update the info directories.

When this variable is a function
it will be called to determine the value when it is needed. When
this variable is a function it will be called to determine the
value when it is needed.")

(defvar stp-auto-lock nil
  "When non-nil, automatically update `stp-lock-file' when packages
are changed. When this variable is a function it will be called
to determine the value when it is needed.")

(defvar stp-never-auto-lock t
  "Never automatically update `stp-lock-file'. This is different
from `stp-auto-lock' because that is just a default argument for
interactive commands.")

(defun stp-ensure-no-merge-conflicts ()
  (when (stp-git-merge-conflict-p)
    (user-error "Merge conflicts must be resolved before running this command")))

(defun stp-unclean-fun ()
  "This function is intended as a value of `stp-allow-unclean'.

It requires the repository to be clean when run inside
`stp-source-directory'. Otherwise, it causes the user to be
prompted."
  (rem-ancestor-of-inclusive-p (stp-git-root) default-directory))

(defvar stp-allow-unclean #'stp-unclean-fun
  "This variable determines the behavior when the git repository is
unclean at the beginning of commands.

When it is nil, an error occurs. :allow means that the command
should proceed without user intervention. If the value is a
function, it will be called with no arguments and the return
value will be interpreted as described here. Any other value
means that the user should be prompted to determine if the
command should proceed.")

(defun stp-maybe-ensure-clean ()
  (let ((unclean (if (functionp stp-allow-unclean)
                     (funcall stp-allow-unclean)
                   stp-allow-unclean)))
    (or (eq unclean :allow)
        (stp-git-clean-p)
        (and (not unclean)
             (user-error "Aborted: the repository is unclean"))
        (yes-or-no-p "The git repo is unclean. Proceed anyway?"))))

(cl-defun stp-command-kwd-args (&key (lock t) actions audit tag (do-commit nil do-commit-provided-p) (do-push nil do-push-provided-p) (do-lock nil do-lock-provided-p) (do-actions nil do-actions-provided-p) (do-audit nil do-audit-provided-p) (do-tag nil do-tag-provided-p) (ensure-clean t) (toggle-p (fn current-prefix-arg)))
  (stp-ensure-no-merge-conflicts)
  (let ((args
         (apply #'append
                (list :do-commit (if do-commit-provided-p do-commit stp-auto-commit)
                      :do-push (if do-push-provided-p do-push stp-auto-push))
                (rem-maybe-args
                 (list :do-lock
                       (if do-lock-provided-p do-lock (and (not stp-never-auto-lock) stp-auto-lock)))
                 lock
                 (list :do-actions (if do-actions-provided-p do-actions stp-auto-post-actions))
                 actions
                 (list :do-audit (if do-audit-provided-p do-audit stp-audit-changes))
                 audit
                 (list :do-tag (if do-tag-provided-p do-tag stp-auto-tag))
                 tag))))
    (when (funcall toggle-p)
      (setq args (stp-toggle-plist "Toggle option: " args)))
    ;; Perform sanity checks.
    (when (and (not (plist-get args :do-commit)) (plist-get args :do-tag))
      (user-error "Tagging without committing is not allowed"))
    (when (and (not (plist-get args :do-commit)) (plist-get args :do-push))
      (user-error "Pushing without committing is not allowed"))
    (when (and ensure-clean (plist-get args :do-commit))
      (stp-maybe-ensure-clean))
    args))

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
  "Return the name of the package on the current line.

When OFFSET is non-nil, return the name of the packages that is
OFFSET lines from the current line or nil if no package
corresponds to that line."
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

(cl-defun stp-list-read-name (prompt &key allow-skip)
  "In `stp-list-mode', return the package on the current line if there
is one. Otherwise, prompt the user for a package."
  (stp-refresh-info)
  (stp-maybe-allow-skip (allow-skip)
    (or (and (derived-mode-p 'stp-list-mode)
             (stp-list-package-on-line))
        (stp-read-existing-name prompt))))

(defvar stp-enforce-min-version nil
  "Determines if the user is allowed to select a version older than
the minimum required by another package.")

(cl-defun stp-command-args (&key pkg-name (prompt-prefix "") pkg-version read-pkg-alist (existing-pkg t) actions audit tag (line-pkg t) min-version enforce-min-version (do-commit nil do-commit-provided-p) (do-push nil do-push-provided-p) (do-lock nil do-lock-provided-p) (do-actions nil do-actions-provided-p) (do-audit nil do-audit-provided-p) (do-tag nil do-tag-provided-p) (toggle-p (fn current-prefix-arg)))
  "Prepare an argument list for an interactive command.

The first argument included in the list is the name of the
package. If PKG-VERSION is non-nil, the \\='VERSION attribute for
the package will be included as the next positional argument. If
READ-PKG-LIST is non-nil, a package alist will be read from the
user and included as an additional positional argument. ACTIONS
determines if the do-actions keyword argument should be included.
Similarly, AUDIT determines if do-audit should be included. TAG
indicates if do-tag should be included. When LINE-PKG is
non-nil (as it is by default), any data that would normally be
read from the user will be inferred from the cursor position when
`stp-list-mode' is active. When non-nil, MIN-VERSION indicates
the minimum version that should be installed. TOGGLE-P is a
function that can be used to check if command options should be
toggled by the user via an interactive menu."
  (stp-with-package-source-directory
    (plet* ((kwd-args (rem-maybe-kwd-args do-commit do-push-provided-p
                                          do-push do-commit-provided-p
                                          do-lock do-lock-provided-p
                                          do-actions do-actions-provided-p
                                          do-audit do-audit-provided-p
                                          do-tag do-tag-provided-p))
            (args (apply #'stp-command-kwd-args :actions actions :audit audit :tag tag :toggle-p toggle-p kwd-args))
            (`(,pkg-name . ,pkg-alist)
             (or (and read-pkg-alist
                      (stp-read-package :pkg-name pkg-name
                                        :prompt-prefix prompt-prefix
                                        :min-version min-version
                                        :enforce-min-version enforce-min-version))
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

(defvar stp-audit-auto-reset t
  "Indicates if git reset should be used to undo changes to the
prior state after an audit fails.")

(defun stp-audit-changes (pkg-name type last-hash)
  (unless (memq type '(install upgrade))
    (error "type must be either 'install or 'upgrade"))
  (stp-git-show-diff (list last-hash))
  (unless (yes-or-no-p "Are the changes to the package safe? ")
    (when stp-audit-auto-reset
      (stp-git-reset last-hash :mode 'hard))
    (signal 'quit
            (list (format "aborted %s %s due to a failed security audit%s"
                          (if (eq type 'install)
                              "installing"
                            "upgrading")
                          pkg-name
                          (if stp-audit-auto-reset
                              ""
                            ": use git reset to undo the suspicious commits"))))))

(defun stp-maybe-audit-changes (pkg-name type last-hash do-audit)
  (when (stp-maybe-call do-audit)
    (stp-audit-changes pkg-name type last-hash)))

(defvar stp-requirements-toplevel t)

(cl-defun stp-install-command (&key pkg-name (prompt-prefix "") min-version allow-skip (do-commit nil do-commit-provided-p) (do-push nil do-push-provided-p) (do-lock nil do-lock-provided-p) (do-actions nil do-actions-provided-p) (do-audit nil do-audit-provided-p) dependency)
  "If `stp-auto-commit', `stp-auto-push', `stp-auto-lock',
`stp-auto-post-actions' and `stp-audit-changes' are non-nil,
commit, push update the lock file, perform post actions (see
`stp-auto-post-actions') and audit the package before running
actions. With a prefix argument, each of these can be toggled via
an interactive menu before running the command.

When ALLOW-SKIP is non-nil, the user is allowed to skip
installing the package. This will result in \\='SKIP being
returned."
  (interactive)
  ;; `stp-install-command' and `stp-install' are separate functions so that
  ;; `stp-command-args' will be called within the same memoization block (which
  ;; greatly improves efficiency).
  (stp-with-package-source-directory
    (stp-with-memoization
      (stp-refresh-info)
      (stp-maybe-allow-skip (allow-skip
                             (stp-msg "Skipped installing %s" (if pkg-name pkg-name "a dependency")))
        (let* ((kwd-args (rem-maybe-kwd-args do-commit do-commit-provided-p do-push do-push-provided-p do-lock do-lock-provided-p do-actions do-actions-provided-p do-audit do-audit-provided-p))
               (args (append (apply #'stp-command-args
                                    :pkg-name pkg-name
                                    :prompt-prefix prompt-prefix
                                    :actions t
                                    :audit t
                                    :read-pkg-alist t
                                    :existing-pkg nil
                                    :line-pkg nil
                                    :min-version min-version
                                    :enforce-min-version stp-enforce-min-version
                                    kwd-args)
                             (list :dependency dependency))))
          (apply #'stp-install args))))))

(cl-defun stp-install (pkg-name pkg-alist &key do-commit do-push do-lock do-actions do-audit dependency (refresh t) (ensure-requirements t))
  "Install a package named PKG-NAME that has the alist PKG-ALIST.

If DO-COMMIT is non-nil, automatically commit to the git
repository after installing the package. If both DO-COMMIT and
DO-PUSH are non-nil, push to the remote repository as well. If
DO-LOCK is non-nil, automatically update `stp-lock-file'. If
DO-ACTIONS is non-nil, call `stp-post-actions' after installing
the package. If DO-AUDIT is non-nil, call `stp-audit-changes'.
DO-COMMIT, DO-PUSH, DO-LOCK, DO-ACTIONS and DO-AUDIT can also be
functions that are called to determine if the associated
operation should be performed.

When DEPENDENCY is non-nil, the package will be marked as a
dependency. When MIN-VERSION is non-nil, it indicates the minimum
version that is required for this package due to installation as
a dependency. When ENSURE-REQUIREMENTS is non-nil, dependencies
of packages are installed or upgraded as needed."
  ;; pkg-name may be nil in interactive calls depending on the value of
  ;; `stp-allow-unclean'. See `stp-maybe-ensure-clean'.
  (when pkg-name
    (setq stp-current-package pkg-name)
    (stp-requirements-initialize-toplevel)
    (let ((last-hash (stp-git-head)))
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
          (stp-maybe-audit-changes pkg-name 'install last-hash do-audit)
          (stp-update-remotes pkg-name .remote .remote .other-remotes)
          (stp-update-requirements pkg-name)
          (when dependency
            (stp-set-attribute pkg-name 'dependency t))
          (stp-write-info)
          ;; For archives, the version is determined automatically instead of
          ;; being read and so .version will be nil here.
          (setq .version (stp-get-attribute pkg-name 'version))
          (stp-git-commit-push (format "Installed version %s of %s"
                                       (stp-abbreviate-remote-version pkg-name .method .remote .version)
                                       pkg-name)
                               :do-commit do-commit
                               :do-push do-push)
          (when ensure-requirements
            (let ((stp-requirements-toplevel nil))
              (ignore stp-requirements-toplevel)
              (stp-ensure-requirements (stp-get-attribute pkg-name 'requirements) :do-commit do-commit :do-actions do-actions)))
          (when (stp-maybe-call do-lock)
            (stp-update-lock-file))
          (when (stp-maybe-call do-actions)
            (stp-post-actions pkg-name))
          (when (and ensure-requirements stp-requirements-toplevel)
            (stp-report-requirements 'install))
          (when refresh
            (stp-update-cached-latest pkg-name)
            (stp-list-refresh :quiet t)))))))

(defun stp-uninstall-command ()
  "Uninstall a package interactively."
  (interactive)
  (stp-with-package-source-directory
    (stp-with-memoization
      (stp-refresh-info)
      (apply #'stp-uninstall (stp-command-args)))))

(cl-defun stp-uninstall (pkg-name &key do-commit do-push do-lock (refresh t) (uninstall-requirements t))
  "Uninstall the package named PKG-NAME.

The arguments DO-COMMIT, DO-PUSH, and DO-LOCK are as in
`stp-install'. UNINSTALL-REQUIREMENTS means that dependencies
that are not needed anymore should also be removed."
  (when pkg-name
    (setq stp-current-package pkg-name)
    (let ((features (stp-headers-directory-features (stp-full-path pkg-name)))
          (requirements (stp-get-attribute pkg-name 'requirements)))
      (let-alist (stp-get-alist pkg-name)
        (if (eql (car (rem-call-process-shell-command (format "git rm -r '%s'" pkg-name))) 0)
            (progn
              (f-delete pkg-name t)
              (stp-delete-alist pkg-name)
              (stp-write-info)
              (cl-dolist (feature features)
                (push feature stp-headers-uninstalled-features))
              (stp-delete-load-path pkg-name)
              (stp-git-commit-push (format "Uninstalled version %s of %s"
                                           (stp-abbreviate-remote-version pkg-name .method .remote .version)
                                           pkg-name)
                                   :do-commit do-commit
                                   :do-push (and (not uninstall-requirements) do-push))
              (when uninstall-requirements
                (stp-maybe-uninstall-requirements requirements :do-commit do-commit)
                (stp-git-push :do-push do-push))
              (when (stp-maybe-call do-lock)
                (stp-update-lock-file))
              (when (and uninstall-requirements stp-requirements-toplevel)
                (stp-report-requirements 'uninstall))
              (when refresh
                (stp-list-refresh :quiet t))
              (stp-prune-cached-latest-versions pkg-name))
          (error "Failed to remove %s. This can happen when there are uncommitted changes in the git repository" pkg-name))))))

(defvar stp-git-upgrade-always-offer-remote-heads t)

(cl-defun stp-upgrade-command (&key pkg-name min-version (prompt-prefix "") allow-skip (do-commit nil do-commit-provided-p) (do-push nil do-push-provided-p) (do-lock nil do-lock-provided-p) (do-actions nil do-actions-provided-p) (do-audit nil do-audit-provided-p))
  "Upgrade a package interactively.

The arguments DO-COMMIT, DO-PUSH, DO-LOCK, DO-ACTIONS and
DO-AUDIT are as in `stp-install'."
  (interactive)
  (stp-with-package-source-directory
    (stp-with-memoization
      (stp-refresh-info)
      (stp-maybe-allow-skip (allow-skip
                             (stp-msg "Skipped upgrading %s" (if pkg-name pkg-name "a dependency")))
        (let* ((kwd-args (rem-maybe-kwd-args do-commit do-push-provided-p do-push do-commit-provided-p do-lock do-lock-provided-p do-actions do-actions-provided-p do-audit do-audit-provided-p))
               (args (append (apply #'stp-command-args
                                    :pkg-name pkg-name
                                    :prompt-prefix prompt-prefix
                                    :actions t
                                    :audit t
                                    :min-version min-version
                                    :enforce-min-version stp-enforce-min-version
                                    kwd-args)
                             (list :min-version min-version
                                   :enforce-min-version stp-enforce-min-version))))
          (apply #'stp-upgrade args))))))

(cl-defun stp-upgrade (pkg-name &key do-commit do-push do-lock do-actions do-audit (refresh t) min-version enforce-min-version (ensure-requirements t))
  (when pkg-name
    (setq stp-current-package pkg-name)
    (stp-requirements-initialize-toplevel)
    (let ((last-hash (stp-git-head)))
      (let-alist (stp-get-alist pkg-name)
        ;; Automatically determine missing other remotes for archive packages.
        (when (eq .method 'archive)
          (setq .other-remotes (cl-set-difference (stp-archives pkg-name) (cons .remote .other-remotes))))
        (let* ((chosen-remote (stp-choose-remote "Remote: " .remote .other-remotes))
               (extra-versions (and (eq .method 'git)
                                    (or stp-git-upgrade-always-offer-remote-heads
                                        (eq .update 'unstable))
                                    (stp-git-remote-heads-sorted chosen-remote)))
               (prompt (format "Upgrade from %s to version%s: "
                               (stp-abbreviate-remote-version pkg-name .method chosen-remote .version)
                               (stp-min-version-annotation min-version enforce-min-version))))
          (when (stp-url-safe-remote-p chosen-remote)
            (when (and .branch (member .branch extra-versions))
              (setq extra-versions (cons .branch (remove .branch extra-versions))))
            (cl-ecase .method
              (git (--> extra-versions
                        (stp-git-read-version
                         prompt
                         chosen-remote
                         :extra-versions-position (if (eq .update 'unstable) 'first 'last)
                         :extra-versions it
                         :branch-to-hash nil
                         :min-version min-version)
                        (stp-git-upgrade pkg-name chosen-remote it)))
              (elpa (->> (stp-elpa-read-version
                          prompt
                          pkg-name
                          chosen-remote
                          :min-version min-version)
                         (stp-elpa-upgrade pkg-name chosen-remote)))
              (archive (stp-archive-upgrade pkg-name .remote))
              (url (->> (stp-url-read-version prompt)
                        (stp-url-upgrade pkg-name chosen-remote))))
            (stp-maybe-audit-changes pkg-name 'upgrade last-hash do-audit)
            ;; The call to `stp-get-attribute' can't be replaced with
            ;; .version because the 'version attribute will have changed
            ;; after the call to `stp-git-upgrade', `stp-elpa-upgrade' or
            ;; `stp-url-upgrade'.
            (let ((new-version (stp-get-attribute pkg-name 'version)))
              (stp-update-remotes pkg-name chosen-remote .remote .other-remotes)
              (stp-update-requirements pkg-name)
              (stp-write-info)
              ;; Don't commit, push or perform push actions when there are
              ;; merge conflicts.
              (if (stp-git-merge-conflict-p)
                  (stp-msg "%s occurred. Please resolve and commit manually."
                           (if (> (length (stp-git-conflicted-files)) 1)
                               "Merge conflicts"
                             "A merge conflict"))
                (stp-git-commit-push (format "Upgraded to version %s of %s"
                                             (stp-abbreviate-remote-version pkg-name .method chosen-remote new-version)
                                             pkg-name)
                                     :do-commit do-commit
                                     :do-push do-push)
                (when ensure-requirements
                  (let ((stp-requirements-toplevel nil))
                    (ignore stp-requirements-toplevel)
                    (stp-ensure-requirements (stp-get-attribute pkg-name 'requirements) :do-commit do-commit :do-actions do-actions)))
                (when (stp-maybe-call do-lock)
                  (stp-update-lock-file))
                (when (stp-maybe-call do-actions)
                  (stp-post-actions pkg-name)))
              (when (and ensure-requirements stp-requirements-toplevel)
                (stp-report-requirements 'upgrade))
              (when refresh
                (stp-update-cached-latest pkg-name)
                (stp-list-refresh :quiet t)))))))))

(defvar stp-failed-requirements nil)
(defvar stp-successful-requirements nil)
(defvar stp-requirements nil)

(defun stp-requirements-initialize-toplevel ()
  (when stp-requirements-toplevel
    (setq stp-failed-requirements nil
          stp-successful-requirements nil
          stp-requirements nil)))

(defun stp-report-requirements (type &optional packages)
  (unless (memq type '(install upgrade uninstall))
    (error "type must be 'INSTALL, 'UPGRADE or 'UNINSTALL"))
  (setq stp-failed-requirements (-uniq stp-failed-requirements)
        stp-successful-requirements (-uniq stp-successful-requirements)
        stp-requirements (-uniq stp-requirements))
  (let* ((total-requirements (+ (length stp-failed-requirements)
                                (length stp-successful-requirements)))
         (noun (cond
                ((and packages (> total-requirements 1)) "packages")
                (packages "package")
                ((> total-requirements 1) "dependencies")
                (t "dependency"))))
    (cond
     (stp-failed-requirements
      (stp-msg "Failed to %s %d/%d %s (see %s):\n%s"
               (if (memq type '(install upgrade))
                   "install or upgrade"
                 "uninstall")
               (length stp-failed-requirements)
               total-requirements
               noun
               stp-log-buffer-name
               (rem-join-and (mapcar (lambda (requirement)
                                       ;; When type is 'uninstall,
                                       ;; `stp-failed-requirements' can be just
                                       ;; the package names instead of proper
                                       ;; requirements.
                                       (if (listp requirement)
                                           (db (pkg-sym &optional version)
                                               (ensure-list requirement)
                                             (format "%s (%s)" (stp-symbol-package-name pkg-sym) version))
                                         requirement))
                                     stp-failed-requirements))))
     ((> total-requirements 0)
      (stp-msg "Successfully %s %d %s"
               (if (memq type '(install upgrade))
                   "installed or upgraded"
                 "uninstalled")
               total-requirements
               noun)))))

(cl-defun stp-ensure-requirements (requirements &key do-commit do-actions (search-load-path t))
  "Install or upgrade each requirement to ensure that at least the
specified version is available. REQUIREMENTS should be a list
where each entry is either the name of a package or a list
containing the name of the package and the minimum version
required."
  (when search-load-path
    (stp-msg "Analyzing the load path for installed packages...")
    (stp-headers-update-features))
  (cl-dolist (requirement requirements)
    ;; Also allow a list of package names.
    (db (pkg-sym &optional version)
        (ensure-list requirement)
      (let* ((pkg-name (stp-symbol-package-name pkg-sym))
             (prefix (format "[%s] " pkg-name)))
        (unless (member pkg-name stp-headers-ignored-requirements)
          (push requirement stp-requirements))
        (condition-case err
            (cond
             ((string= pkg-name "emacs")
              (unless (stp-emacs-requirement-satisfied-p pkg-name version)
                (error "Version %s of Emacs is required but %d.%d is installed"
                       version
                       emacs-major-version
                       emacs-minor-version)))
             ;; Do nothing when a requirement is ignored or a new enough
             ;; version is installed.
             ((stp-package-requirement-satisfied-p pkg-name version t))
             ((not (member pkg-name (stp-info-names)))
              ;; Sometimes, a single repository can contain multiple packages
              ;; and so installing the dependencies naively will result in
              ;; multiple copies. :allow-skip t is passed so that the user can
              ;; skip these if desired.
              (unless (eq (stp-install-command :pkg-name pkg-name
                                               :prompt-prefix prefix
                                               :min-version version
                                               :do-commit do-commit
                                               :do-push nil
                                               :do-lock nil
                                               :do-actions do-actions
                                               :dependency t
                                               :allow-skip t)
                          'skip)
                (push requirement stp-successful-requirements)))
             (t
              ;; The dependency attribute is left as is when upgrading because
              ;; the package might have been installed manually originally.
              (unless (eq (stp-upgrade-command :pkg-name pkg-name
                                               :prompt-prefix prefix
                                               :min-version version
                                               :do-commit do-commit
                                               :do-push nil
                                               :do-lock nil
                                               :do-actions do-actions
                                               :allow-skip t)
                          'skip)
                (push requirement stp-successful-requirements)
                (when (stp-git-merge-conflict-p)
                  (stp-msg "One or more merge conflicts occurred while upgrading. Resolve the conflict and then press M-x `exit-recursive-edit'")
                  (recursive-edit)))))
          (error
           (push requirement stp-failed-requirements)
           (stp-msg "Failed to install or upgrade %s%s: %s"
                    pkg-name
                    (if version
                        (format " to version %s" version)
                      "
                        ")
                    err)))))
    (when search-load-path
      (stp-headers-update-features))))

(cl-defun stp-maybe-uninstall-requirements (requirements &key do-commit)
  (let* ((to-uninstall (stp-requirements-to-names requirements))
         (old-to-uninstall t)
         pkg-name)
    (while to-uninstall
      (when (equal to-uninstall old-to-uninstall)
        (error "Recursive dependencies encountered while uninstalling packages"))
      (condition-case err
          (progn
            (setq old-to-uninstall (cl-copy-list to-uninstall)
                  pkg-name (stp-symbol-package-name (pop to-uninstall)))
            ;; Only uninstall STP packages that were installed as dependencies and
            ;; are no longer required by any package.
            (when (and (member pkg-name (stp-info-names))
                       (stp-get-attribute pkg-name 'dependency)
                       (not (stp-required-by pkg-name)))
              (let ((recursive-requirements (stp-get-attribute pkg-name 'requirements)))
                (push pkg-name stp-requirements)
                (stp-uninstall pkg-name :do-commit do-commit :uninstall-requirements nil)
                (setq to-uninstall (cl-union to-uninstall
                                             (stp-requirements-to-names recursive-requirements)
                                             :test #'string=)))))
        (error
         (push pkg-name stp-failed-requirements)
         (stp-msg "Failed to uninstall %s: %s" pkg-name err))))))

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

(cl-defun stp-package-group-command (fun table &key extra-removed-kwargs)
  (stp-refresh-info)
  (stp-requirements-initialize-toplevel)
  (let* ((pkg-names (-> (stp-read-existing-name "Group or package name: "
                                                :table table
                                                :multiple t)
                        stp-expand-groups))
         (args (stp-command-kwd-args :actions t))
         (args2 (map-into (map-remove (fn2 (memq %1 (append '(:do-push :do-lock) extra-removed-kwargs))) args) 'plist)))
    (funcall fun pkg-names args2)
    (stp-git-push :do-push (map-elt args :do-push))
    (when (stp-maybe-call (map-elt args :do-lock))
      (stp-update-lock-file))))

(defun stp-install-or-upgrade-package-group-command ()
  "Install or upgrade the package groups or packages."
  (interactive)
  (stp-with-package-source-directory
    (stp-with-memoization
      (stp-requirements-initialize-toplevel)
      (let ((stp-requirements-toplevel nil)
            (table (completion-table-in-turn (stp-get-info-group-names)
                                             (stp-info-names)
                                             (stp-archive-package-names))))
        (stp-package-group-command (lambda (pkg-names args)
                                     (apply #'stp-ensure-requirements
                                            pkg-names
                                            args))
                                   table)
        (stp-report-requirements 'install t)))))

(defun stp-uninstall-package-group-command ()
  "Uninstall the package groups or packages."
  (interactive)
  (stp-with-package-source-directory
    (stp-with-memoization
      (stp-requirements-initialize-toplevel)
      (let ((stp-requirements-toplevel nil)
            (table (completion-table-in-turn (stp-get-info-group-names) (stp-info-names))))
        (stp-package-group-command (lambda (pkg-names args)
                                     (apply #'stp-maybe-uninstall-requirements
                                            pkg-names
                                            args))
                                   table)
        (stp-report-requirements 'uninstall t)))))

(defvar stp-fork-directory nil
  "The directory to use for forks. When this is nil,
`stp-source-directory' is used.")

(defun stp-fork-command ()
  (interactive)
  (stp-refresh-info)
  (let ((pkg-name (stp-list-read-name "Package name: ")))
    (let-alist (stp-get-alist pkg-name)
      (unless (eq .method 'git)
        (user-error "Only packages that use the git method can be forked"))
      (let* ((dir (or stp-fork-directory stp-source-directory))
             (remote (stp-choose-remote "Remote: " .remote .other-remotes)))
        (apply #'stp-fork pkg-name remote dir (stp-command-kwd-args :lock nil))))))

(cl-defun stp-fork (pkg-name remote dir &key do-commit do-push)
  (setq stp-current-package pkg-name)
  (let-alist (stp-get-alist pkg-name)
    (unless (eq .method 'git)
      (error "Only packages that use the git method can be forked"))
    (unless (executable-find "gh")
      (error "gh is required for forking"))
    (let ((default-directory dir))
      (rem-run-command (-uniq (list "gh" "repo" "fork" "--clone" "--fork-name" pkg-name remote)))
      (let* ((default-directory (f-join dir pkg-name))
             (remotes (-uniq (cl-list* (map-elt (stp-git-remotes) "origin")
                                       remote
                                       .remote
                                       .other-remotes))))
        (stp-set-attribute pkg-name 'remote (car remotes))
        (stp-set-attribute pkg-name 'other-remotes (cdr remotes))
        (stp-with-package-source-directory
          (stp-write-info)
          (stp-git-commit-push (format "Added the remote for the fork of %s" pkg-name) :do-commit do-commit :do-push do-push))))))

(defun stp-download-url (pkg-name pkg-alist)
  (let-alist pkg-alist
    ;; Note that for the 'git method there is no download URL.
    (cl-ecase .method
      (elpa
       (stp-elpa-download-url pkg-name .remote .version))
      (archive
       ;; .remote is a symbol representing the archive for the 'archive method.
       (stp-archive-download-url pkg-name .remote))
      (url
       .remote))))

(cl-defun stp-reinstall-command ()
  "Uninstall and reinstall a package interactively as the same version."
  (interactive)
  (stp-with-package-source-directory
    (stp-with-memoization
      (stp-refresh-info)
      (apply #'stp-reinstall (stp-command-args :pkg-version t :actions t :audit t)))))

(cl-defun stp-reinstall (pkg-name version &key do-commit do-push do-lock do-actions do-audit refresh skip-subtree-check)
  "Uninstall and reinstall PKG-NAME as VERSION.

The DO-COMMIT, DO-PUSH, DO-LOCK, DO-ACTIONS and DO-AUDIT
arguments are as in `stp-install'."
  (when (and (stp-git-tree-package-modified-p pkg-name)
             (not (yes-or-no-p (format "The package %s has been modified since the last commit in the working tree. Reinstalling will delete these changes. Do you wish to proceed?" pkg-name))))
    (user-error "Reinstall aborted"))
  (let-alist (stp-get-alist pkg-name)
    (let* ((pkg-alist (stp-get-alist pkg-name))
           (tree-hashes (and (not skip-subtree-check)
                             (if (eq .method 'git)
                                 (stp-git-subtree-package-modified-p pkg-name .remote .version)
                               ;; For methods other than 'git, we need to create
                               ;; a synthetic git repository for comparision
                               ;; purposes.
                               (stp-git-subtree-package-modified-p pkg-name (stp-git-download-as-synthetic-repo pkg-name (stp-download-url pkg-name pkg-alist)) "HEAD")))))
      ;; Warn the user about reinstalling if there are modifications to the
      ;; subtree that were not the result of git subtree merge as this will
      ;; result in the loss of their customizations to the package.
      (save-window-excursion
        (when (and tree-hashes
                   (unwind-protect
                       ;; curr-hash is the hash of the most recent version of
                       ;; the subtree (which may include user modifications).
                       ;; last-hash is the hash of the last subtree that was
                       ;; merged (e.g. by installing or upgrading the package).
                       (and (db (curr-hash last-hash)
                                tree-hashes
                              (stp-git-show-diff (list last-hash curr-hash))
                              t)
                            (not (yes-or-no-p (format "The package %s has been modified locally. Reinstalling will delete these changes. Do you wish to proceed?" pkg-name))))
                     (awhen (get-buffer stp-git-diff-buffer-name)
                       (bury-buffer it))
                     (redisplay)))
          (user-error "Reinstall aborted")))
      ;; Committing is required here because otherwise `stp-install' will fail.
      ;; Refreshing the stp-list-buffer-name buffer is suppressed since that
      ;; will be done by stp-upgrade (which calls this command).
      (stp-uninstall pkg-name :do-commit t :refresh nil)
      (setf (map-elt pkg-alist 'version) version)
      ;; The :do-commit argument is not required here. The decisions to
      ;; commit, push or perform post actions will be handled at a
      ;; higher level by `stp-upgrade'.
      (stp-install pkg-name pkg-alist :do-commit do-commit :do-push do-push :do-actions do-actions :do-audit do-audit :do-lock do-lock :refresh refresh))))

(defun stp-add-or-edit-package-group-command ()
  "Add or edit a package group for easily upgrading multiple related
packages at the same time."
  (interactive)
  (stp-ensure-no-merge-conflicts)
  (stp-with-package-source-directory
    (stp-with-memoization
      (stp-refresh-info)
      (let* ((group-name (stp-read-group-name "Group: "))
             (pkg-names (stp-get-info-group group-name))
             (table (completion-table-in-turn pkg-names (stp-info-names)))
             (args (stp-command-kwd-args)))
        (apply #'stp-add-or-edit-package-group
               group-name
               (stp-read-existing-name "Package name: "
                                       :multiple t
                                       :table table)
               args)))))

(cl-defun stp-add-or-edit-package-group (group-name pkg-names &key do-commit do-push do-lock)
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
      (stp-update-lock-file))))

(defun stp-delete-package-group-command ()
  "Remove a package group."
  (interactive)
  (stp-ensure-no-merge-conflicts)
  (stp-with-memoization
    (stp-refresh-info)
    (let ((args (stp-command-kwd-args)))
      (apply #'stp-delete-package-group
             (stp-read-group-name "Group: ")
             args))))

(cl-defun stp-delete-package-group (group-name &key do-commit do-push do-lock)
  (stp-delete-info-group group-name)
  (stp-write-info)
  (stp-git-commit-push (format "Deleted the package group %s" group-name)
                       :do-commit do-commit
                       :do-push do-push)
  (when (stp-maybe-call do-lock)
    (stp-update-lock-file)))

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
        (apply #'stp-repair (stp-command-args :toggle-p (fn (consp current-prefix-arg))))))))

(cl-defun stp-repair (pkg-name &key do-commit do-push do-lock (refresh t))
  "Repair the package named pkg-name.

The DO-COMMIT, DO-PUSH AND DO-LOCK arguments are as in
`stp-install'."
  (when pkg-name
    (stp-repair-info :quiet nil :pkg-names (list pkg-name))
    (stp-write-info)
    (stp-git-commit-push (format "Repaired the source package %s" pkg-name)
                         :do-commit do-commit
                         :do-push do-push)
    (when (stp-maybe-call do-lock)
      (stp-update-lock-file))
    (when refresh
      (stp-list-refresh :quiet t))))

(cl-defun stp-repair-all-command (&key (toggle-p nil toggle-p-provided-p))
  (interactive)
  (stp-with-package-source-directory
    (stp-with-memoization
      (stp-refresh-info)
      (apply #'stp-repair-all (apply #'stp-command-kwd-args
                                     (rem-maybe-kwd-args toggle-p toggle-p-provided-p))))))

(cl-defun stp-repair-all (&key do-commit do-push do-lock (refresh t))
  "Repair the stored package information for all packages."
  (stp-repair-info :quiet nil)
  (stp-write-info)
  (stp-git-commit-push (format "Repaired source packages")
                       :do-commit do-commit
                       :do-push do-push)
  (when (stp-maybe-call do-lock)
    (stp-update-lock-file))
  (when refresh
    (stp-list-refresh :quiet t)))

(defun stp-edit-remotes-command ()
  "Edit the stored remotes of a package."
  (interactive)
  (stp-ensure-no-merge-conflicts)
  (stp-with-memoization
    (stp-refresh-info)
    (apply #'stp-edit-remotes (stp-command-args))))

(defvar stp-edit-remotes-long-commit-msg nil)

(cl-defun stp-edit-remotes (pkg-name &key do-commit do-push do-lock (refresh t))
  "Edit the remote and other-remotes attributes of PKG-NAME.

This uses `completing-read-multiple'. The first chosen will be
remotes and the rest will be other-remotes. The arguments
DO-COMMIT, DO-PUSH, and DO-LOCK are as in `stp-install'."
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
          (setq stp-current-package pkg-name)
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
          (when (stp-maybe-call do-lock)
            (stp-update-lock-file))
          (when refresh
            (stp-list-refresh :quiet t)))
      (stp-msg "There are no remotes to edit%s"
               (if pkg-name
                   (format "for %s" pkg-name)
                 "")))))

(defun stp-toggle-update-command ()
  "Toggle the update attribute of a package interactively."
  (interactive)
  (stp-with-memoization
    (stp-refresh-info)
    (apply #'stp-toggle-update (stp-command-args))))

(cl-defun stp-toggle-update (pkg-name &key do-commit do-push do-lock (refresh t))
  "Toggle the update attribute for the package named PKG-NAME.

The arguments DO-COMMIT, DO-PUSH and DO-LOCK are as in
`stp-install'."
  (when pkg-name
    (setq stp-current-package pkg-name)
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
                                 :do-commit do-commit
                                 :do-push do-push)
            (when (stp-maybe-call do-lock)
              (stp-update-lock-file))
            (when refresh
              (stp-list-refresh :quiet t)))
        (user-error "The update attribute can only be toggled for git packages.")))))

(defun stp-toggle-dependency-command ()
  "Toggle the dependency attribute of a package interactively.


This indicates that the packages was installed because another
package requires it rather than explicitly by the user."
  (interactive)
  (apply #'stp-toggle-dependency (stp-command-args)))

(cl-defun stp-toggle-dependency (pkg-name &key do-commit do-push do-lock)
  (when pkg-name
    (setq stp-current-package pkg-name)
    (let ((dependency (stp-get-attribute pkg-name 'dependency)))
      (if dependency
          (stp-delete-attribute pkg-name 'dependency)
        (stp-set-attribute pkg-name 'dependency t))
      (stp-write-info)
      (stp-git-commit-push (format "Changed dependency to %s for %s"
                                   (not dependency)
                                   pkg-name)
                           :do-commit do-commit
                           :do-push do-push)
      (when (stp-maybe-call do-lock)
        (stp-update-lock-file)))))

(defun stp-post-actions-command ()
  "Perform actions for newly installed or upgraded packages.

These include building, updating info directories loading the
package and updating the load path."
  (interactive)
  (stp-with-memoization
    (stp-refresh-info)
    (stp-post-actions (stp-list-read-name "Package name: "))))

(defun stp-post-actions (pkg-name)
  (setq stp-current-package pkg-name)
  (when (stp-maybe-call stp-auto-update-load-path)
    (stp-update-load-path (stp-full-path pkg-name)))
  (when (stp-maybe-call stp-auto-load)
    (condition-case err
        (stp-reload pkg-name)
      (error (display-warning 'STP "Error while loading %s modules: %s" pkg-name (error-message-string err)))))
  (when (stp-maybe-call stp-auto-build)
    (stp-build pkg-name))
  (when (stp-maybe-call stp-auto-build-info)
    (stp-build-info pkg-name))
  (when (stp-maybe-call stp-auto-update-info-directories)
    (stp-update-info-directories pkg-name)))

(defun stp-update-lock-file (&optional interactive-p)
  "Write the hash of the git repository to the lock file."
  (interactive (list t))
  (stp-with-package-source-directory
    (let ((hash (stp-git-rev-to-hash stp-source-directory "HEAD")))
      (with-temp-buffer
        (insert (format "%S\n" hash))
        (f-write (buffer-string) 'utf-8 stp-lock-file)
        (when interactive-p
          (stp-msg "Updated the lock file at %s" stp-lock-file))))))

(defun stp-lock-file-watcher (event)
  (let ((action (cadr event)))
    (when (memq action '(created changed))
      (stp-checkout-locked-revision))))

(defvar stp-build-output-buffer-name "*STP Build Output*")

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

(defun stp-build (pkg-name &optional allow-naive-byte-compile)
  "Build the package PKG-NAME.

This is done by running the appropriate build systems or
performing naive byte compilation. Return non-nil if there were
no errors."
  (when pkg-name
    (setq stp-current-package pkg-name)
    (let* ((output-buffer stp-build-output-buffer-name)
         (pkg-path (stp-canonical-path pkg-name))
         (build-dir pkg-path))
    ;; Setup output buffer
    (get-buffer-create output-buffer)
    ;; Handle CMake separately. Since it generates makefiles, make may need
    ;; to be run afterwards.
    (when (f-exists-p (f-expand "CMakeLists.txt" pkg-path))
      (stp-msg "CMakeLists.txt was found in %s. Attempting to run cmake..." build-dir)
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
        (let ((cmd '("cmake" "..")))
          (stp-before-build-command cmd output-buffer)
          ;; This will use `build-dir' as the build directory and
          ;; `pkg-path' as the source directory so there is no
          ;; ambiguity as to which CMakeLists.txt file should be
          ;; used.
          (unless (eql (rem-run-command cmd :buffer output-buffer) 0)
            (stp-msg "Failed to run cmake on %s" build-dir)))))
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
                   (stp-msg "A makefile was found in %s. Attempting to run make..." build-dir)
                   (let ((cmd '("make")))
                     (stp-before-build-command cmd output-buffer)
                     ;; Make expects a makefile to be in the current directory
                     ;; so there is no ambiguity over which makefile will be
                     ;; used.
                     (or (eql (rem-run-command cmd :buffer output-buffer) 0)
                         (and (stp-msg "Failed to run make on %s" pkg-path)
                              nil)))))
               (and allow-naive-byte-compile
                    (let ((default-directory pkg-path))
                      (stp-msg "Attempting to byte compile files in %s..." pkg-path)
                      (condition-case err
                          (progn
                            ;; Put the messages from `byte-recompile-directory' in
                            ;; output-buffer.
                            (dflet ((stp-msg (&rest args)
                                             (with-current-buffer output-buffer
                                               (insert (apply #'format args)))))
                              (stp-before-build-command "Byte compiling files" output-buffer)
                              ;; Packages have to be compiled and loaded twice
                              ;; to ensure that macros will work.
                              (byte-recompile-directory pkg-path 0)
                              (stp-reload-once pkg-name)
                              (byte-recompile-directory pkg-path 0)
                              (stp-reload-once pkg-name))
                            t)
                        (error (ignore err)
                               (stp-msg "Byte-compiling %s failed" pkg-path)
                               nil)))))))
      ;; Return success or failure
      (if success
          (stp-msg "Successfully built %s" pkg-name)
        (stp-msg "Build failed for %s" pkg-name))
      success))))

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

(defun stp-build-info (pkg-name)
  "Build the info manuals for PKG-NAME."
  (interactive (list (stp-list-read-name "Package name: ")))
  (when pkg-name
    (setq stp-current-package pkg-name)
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
              (cl-dolist (makefile makefiles)
                (when (member target (stp-make-targets makefile))
                  (let ((default-directory (f-dirname makefile)))
                    (setq attempted t)
                    (stp-msg "Makefile with target %s found in %s. Attempting to run make..." target (f-dirname makefile))
                    (let ((cmd (list "make" target)))
                      (stp-before-build-command cmd output-buffer)
                      (if (eql (rem-run-command cmd :buffer output-buffer) 0)
                          (progn
                            (stp-msg "Built the info manual for %s using make" pkg-name)
                            (cl-return t))
                        (stp-msg "'%s' failed in %s" cmd (f-dirname makefile)))))))

              ;; Try to compile a texi file directly.
              (cl-dolist (source (f-entries (stp-canonical-path pkg-name)
                                         (lambda (path)
                                           (string= (f-filename path) texi-target))
                                         t))
                (let ((default-directory (f-dirname source)))
                  (setq attempted t)
                  (stp-msg "texi source file found at %s. Attempting to compile it with makeinfo..." source)
                  (let ((cmd (list "makeinfo" "--no-split" texi-target)))
                    (cond
                     (;; Don't build texi files unless they have changed since the info
                      ;; manual was last built.
                      (f-newer-p (f-swap-ext source "info") source)
                      (stp-msg "The info manual for %s is up to date" pkg-name)
                      (cl-return t))
                     ((progn
                        (stp-before-build-command cmd output-buffer)
                        (eql (rem-run-command cmd :buffer output-buffer) 0))
                      (stp-msg "Built the info manual for %s using makeinfo" pkg-name)
                      (cl-return t))
                     (t
                      (stp-msg "'%s' failed" cmd)))))))))
    (unless attempted
      (stp-msg "No makefiles or texi source files found for the %s info manual" pkg-name))
    success)))

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

(cl-defun stp-reload (pkg-name &key quiet)
  "Reload the package."
  (interactive (list (stp-list-read-name "Package name: ")))
  ;; Reload the package twice so that macros are handled properly.
  (stp-reload-once pkg-name)
  (stp-reload-once pkg-name)
  (unless quiet
    (stp-msg "Reloaded %s" pkg-name)))

(cl-defun stp-list-update-load-path (&optional arg)
  "Reload the package."
  (interactive "P")
  (if arg
      (stp-update-load-paths t)
    (stp-update-load-path (stp-canonical-path (stp-list-read-name "Package name: ")) t)))

(defun stp-update-info-directories (pkg-name &optional quiet)
  "Make the info files for PKG-NAME available to info commands."
  (interactive (list (stp-list-read-name "Package name: ")))
  (when pkg-name
    (setq stp-current-package pkg-name)
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
            (stp-msg "Added info files for %s" pkg-name)
          (stp-msg "No info files found for %s" pkg-name))))))

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

(defun stp-latest-version (pkg-name &optional pkg-alist)
  (setq pkg-alist (or pkg-alist (stp-get-alist pkg-name)))
  (let-alist pkg-alist
    (let ((remotes (cons .remote .other-remotes))
          (timestamp (float-time)))
      (cl-ecase .method
        (git
         ;; Use `cl-find' to make sure that the branch exists on the remote and
         ;; otherwise default to HEAD. This can be an issue when there is a
         ;; local fork of an upstream repository that the packages was upgraded
         ;; to (for example with a new local branch for a pull request).
         (let* ((branch (or (cl-find .branch
                                     (stp-git-remote-heads .remote)
                                     :test #'string=)
                            "HEAD"))
                ;; Only use the remote attribute to determining the latest versions.
                (latest-stable (stp-git-latest-stable-version .remote))
                (latest-unstable (->> branch
                                      (stp-git-latest-unstable-version .remote)
                                      (stp-git-remote-rev-to-tag .remote)))
                ;; Use both the remote attribute and any other remotes to count
                ;; commits. This is important since the installed version might
                ;; be from any remote.
                (commits-to-stable (and latest-stable
                                        (stp-git-count-remote-commits remotes .version latest-stable)))
                (commits-to-unstable (and latest-unstable
                                          (stp-git-count-remote-commits remotes .version latest-unstable)))
                (version-timestamp (and .version (stp-git-remote-timestamp remotes .version)))
                (stable-timestamp (and latest-stable (stp-git-remote-timestamp remotes latest-stable)))
                (unstable-timestamp (and latest-unstable (stp-git-remote-timestamp remotes latest-unstable))))
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
  "The number of processes for computing the latest versions.

This only has an effect when the latest versions are computed
asynchronously. See `stp-latest-version-async'.")

(defvar stp-latest-retries 3
  "Retry computing latest versions up to this many times.")

(cl-defun stp-latest-versions (package-callback final-callback pkg-names &key quiet async (num-processes stp-latest-num-processes) (max-tries stp-latest-retries))
  "Compute the latest versions for the packages in PACKAGES.

Once the latest version becomes available for package, call
PACKAGE-CALLBACK with the latest version alist as the argument.
Once all latest versions are available, call FINAL-CALLBACK with
the alist mapping the names of the packages to their latest
version alists. he latest versions are computed asynchronously
using NUM-PROCESSES simultaneously. In case an error occurs while
computing the latest version for a package, it will be retried up
to TRIES times."
  (let (latest-versions
        (queue (make-queue))
        (running 0))
    (cl-dolist (pkg-name pkg-names)
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
                       (stp-msg "Getting the latest version of %s failed %d times: skipping..." pkg-name tries))
                   (cl-incf tries)
                   (unless quiet
                     (stp-msg "Getting the latest version of %s failed (%d/%d): %s" pkg-name tries max-tries error-message))
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
                   ;; Binding `async-prompt-for-password' to nil avoids a bug on
                   ;; certain packages (in particular password-store).
                   (let ((async-prompt-for-password nil))
                     (async-start `(lambda ()
                                     ;; Inject the STP variables and the
                                     ;; caller's load path into the asynchronous
                                     ;; process. Some large variables are
                                     ;; excluded since that slows down the
                                     ;; parent process quite a bit.
                                     ,(async-inject-variables "^stp-" nil (concat stp-async-inject-variables-exclude-regexp stp-async-inject-large-variables-exclude-regexp))
                                     (setq load-path ',load-path)
                                     (require 'stp)
                                     ;; pkg-alist is read from disk every time
                                     ;; rather than stored in case some other
                                     ;; STP command (such as an upgrade has
                                     ;; modified the package information while
                                     ;; the latest versions were being updated).
                                     (stp-with-memoization
                                       (stp-refresh-info)
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
          (compute-next-latest-version))))))

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
refresh `package-archive-contents' asynchronously."
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

(defun stp-delete-unnecessary-dependencies (&key do-commit do-push)
  "Uninstall packages that were installed as dependencies but are no
longer required by any other package."
  (interactive (stp-command-kwd-args :lock nil))
  (stp-refresh-info)
  (let ((pkgs (stp-find-unnecessary-dependencies)))
    (stp-maybe-uninstall-requirements pkgs :do-commit do-commit)
    (stp-git-push :do-push do-push)))

(cl-defun stp-bump-version (filename &key do-commit do-push do-tag)
  "Increase the version header for FILENAME. Interactively, this is
the file for the current buffer or the main file for the package
if no version header is found for the current file."
  (interactive (cons (cl-flet ((has-version-header-p (filename)
                                 (when filename
                                   (when (functionp filename)
                                     (setq filename (funcall filename)))
                                   (with-temp-buffer
                                     (insert-file-contents filename)
                                     (and (stp-headers-version) filename)))))
                       (cl-some #'has-version-header-p
                                (list (buffer-file-name (buffer-base-buffer))
                                      ;; `stp-main-package-file' can prompt the user so we don't want
                                      ;; to actually call it unless it's really necessary.
                                      (fn (aand (stp-git-root)
                                                (stp-main-package-file it)))
                                      (fn (user-error "No Version header was found")))))
                     (stp-command-kwd-args :lock nil :tag t :ensure-clean nil)))
  (when (stp-maybe-call do-commit)
    (stp-maybe-ensure-clean))
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
            ;; Give the user a chance to use their own stp-msg if we aren't just
            ;; bumping the version in this commit.
            (unless clean
              (setq msg (rem-read-from-mini "Commit message: " :initial-contents msg)))
            (stp-git-commit msg :do-commit t))
          (when (stp-maybe-call do-tag)
            (let ((tag (concat "v" new-version)))
              (stp-git-tag tag (stp-git-head default-directory))
              (stp-msg "Added the git tag %s for %s" tag (stp-git-root :transform #'f-full))))
          (stp-git-push :do-push do-push)
          (stp-git-push :do-push do-push :tags t))))))

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
  (unless stp-headers-installed-features
    (stp-headers-update-features))
  (stp-savehist-setup))

(provide 'subtree-package)
;;; subtree-package.el ends here

;; Local Variables:
;; eval: (add-hook 'stp-headers-update-hook #'stp-headers-write-bootstrap-requirements nil t)
;; End:
