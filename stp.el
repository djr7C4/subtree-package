;;; -*- lexical-binding: t; -*-

;;; Source packages
(require 'find-lisp)
(require 'info)
(require 'stp-bootstrap)
(require 'stp-utils)
(require 'stp-elpa)
(require 'stp-git)
(require 'stp-url)
(require 'text-property-search)
(require 'url-handlers)
(require 'xml)

(defun stp-remote-method (remote)
  "Determine the method for the remote."
  (cond
   ((stp-git-valid-remote-p remote) 'git)
   ((stp-elpa-valid-remote-p remote) 'elpa)
   ((stp-url-valid-remote-p remote) 'url)
   (t (error "Invalid remote: %s" remote))))

(defvar stp-remote-history nil)

(defun stp-read-remote (prompt &optional default)
  "Read any type of remote."
  (stp-read-remote-with-predicate prompt
                                  (lambda (remote)
                                    (-any-p (lambda (predicate)
                                              (funcall predicate remote))
                                            (list #'stp-git-valid-remote-p
                                                  #'stp-elpa-valid-remote-p
                                                  #'stp-url-valid-remote-p)))
                                  default
                                  'stp-remote-history))

(cl-defun stp-read-package (&key pkg-name pkg-alist prompt-prefix (line-pkg t))
  (let* ((remote (stp-read-remote (concat prompt-prefix "Remote: ") (alist-get 'remote pkg-alist)))
         (pkg-name (or pkg-name (stp-read-name "Package name: " (stp-default-name remote))))
         (method (stp-remote-method remote))
         version
         update
         branch)
    (cl-ecase method
      (git
       (unless (stp-git-valid-remote-p remote)
         (user-error "Invalid git repository (or host is down): %s" remote))
       (unless update
         (setq update (stp-git-read-update (concat prompt-prefix "Update policy: ") (alist-get 'update pkg-alist))))
       (when (and (eq update 'unstable)
                  (not branch))
         (setq branch (stp-git-read-branch (concat prompt-prefix "Branch: ") remote (alist-get 'branch pkg-alist))))
       (unless version
         (setq version (stp-git-read-version (concat prompt-prefix "Version: ") remote :extra-versions (list (alist-get 'version pkg-alist) branch) :default (alist-get 'version pkg-alist)))))
      ((elpa url)
       (when (or (not (string-match-p rem-strict-url-regexp remote))
                 (not (url-file-exists-p remote)))
         (user-error "Invalid URL (or host is down): %s" remote))
       (unless version
         (cl-ecase method
           (elpa (setq version (stp-elpa-read-version (concat prompt-prefix "Version: ") pkg-name remote (alist-get 'version pkg-alist))))
           (url (setq version (stp-url-read-version (concat prompt-prefix "Version: ") (alist-get 'version pkg-alist))))))))
    (cons pkg-name
          (if (eq method 'git)
              `((method . ,method)
                (remote . ,remote)
                (version . ,version)
                (update . ,update)
                (branch . ,branch))
            `((method . ,method)
              (remote . ,remote)
              (version . ,version))))))

(defvar stp-use-other-remotes t
  "If this variable is non-nil, allow the user to choose from the
other-remotes attribute in `stp-info-file' when a remote needs to
be selected.")

(defvar stp-remote-history nil)

(defun stp-choose-remote (prompt remote &optional other-remotes)
  (if stp-use-other-remotes
      ;; A match is not required. This way, a new remote can be added
      ;; interactively by the user. Type ./ to complete in the current
      ;; directory.
      (let ((known-remotes (cons remote other-remotes)))
        (rem-comp-read prompt
                       (completion-table-in-turn known-remotes
                                                 #'completion-file-name-table)
                       :predicate (lambda (candidate)
                                    (or (member candidate known-remotes)
                                        (f-dir-p candidate)))
                       :default remote
                       :history 'stp-remote-history
                       :sort-fun #'identity))
    remote))

(defun stp-update-remotes (pkg-info pkg-name chosen-remote remote other-remotes)
  ;; Remote should always be chosen-remote since that is where the package was
  ;; just installed or upgraded from. (See the documentation of
  ;; `stp-info-file'.) Other-remotes is whatever other remotes exist that were
  ;; not chosen.
  (stp-set-attribute pkg-info pkg-name 'remote chosen-remote)
  (->> (cons remote other-remotes)
       (remove chosen-remote)
       (stp-set-attribute pkg-info pkg-name 'other-remotes)))

(defun stp-repair-default-callback (type pkg-info pkg-name)
  (cl-flet ((handle-partial-elpa-url (pkg-info pkg-name)
              (stp-set-alist pkg-info
                             pkg-name
                             (stp-read-package :pkg-name pkg-name :pkg-alist (stp-get-alist pkg-info pkg-name)))))
    (cl-case type
      (ghost-package (yes-or-no-p (format "%s was found in %s but not in the filesystem in %s. Remove it?" pkg-name stp-info-file stp-source-directory)))
      (invalid-git-remote (stp-git-read-remote (format "The remote %s for %s is invalid or temporarily unavailable; enter remote: " (stp-get-attribute pkg-info pkg-name 'remote) pkg-name)))
      (unknown-git-version (stp-git-read-version (format "Unable to determine verion for %s; enter version: " pkg-name)
                                                 (stp-get-attribute pkg-info pkg-name 'remote)
                                                 :extra-versions (list (stp-get-attribute pkg-info pkg-name 'branch))))
      (unknown-git-update (stp-git-read-update (format "Unable to determine update for %s; enter update: " pkg-name)))
      (unknown-git-branch (stp-git-read-branch (format "Unable to determine branch for %s; enter branch: " pkg-name) (stp-get-attribute pkg-info pkg-name 'remote)))
      (partial-elpa-package (handle-partial-elpa-url pkg-info pkg-name))
      (partial-url-package (handle-partial-elpa-url pkg-info pkg-name))
      (unknown-package (stp-set-alist pkg-info pkg-name (cdr (stp-read-package :pkg-name pkg-name :prompt-prefix (format "Package info is missing for %s; " pkg-name)))))
      ;; This callback ensures that the `stp-info-file' is updated after
      ;; each package is repaired. This is helpful in case there is an error.
      (pkg-info-updated (stp-write-info pkg-info)))))

(defun stp-valid-remote-p (remote)
  (or (stp-git-valid-remote-p remote)
      (stp-elpa-valid-remote-p remote)
      (stp-url-valid-remote-p remote)))

(cl-defun stp-repair-info (pkg-info &key (quiet t) (pkg-names (stp-filesystem-names)) (callback #'stp-repair-default-callback))
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
          (let ((pkg-name (stp-name pkg-name))
                (method (stp-get-attribute pkg-info pkg-name 'method)))
            (unless quiet
              (message (concat (if (> n 1)
                                   (format "(%d/%d) " i n)
                                 "")
                               "Analyzing %s...")
                       pkg-name))
            (if (f-dir-p pkg-name)
                (progn
                  (unless method
                    ;; This means that pkg-name exists in `stp-source-directory'
                    ;; but is not recorded in pkg-info. In other words, the user
                    ;; installed the package manually without using
                    ;; `stp-install'.
                    (when (stp-git-subtree-p pkg-name)
                      (setq method 'git
                            pkg-info (stp-set-attribute pkg-info pkg-name 'method 'git))))
                  (let* ((other-remotes (stp-get-attribute pkg-info pkg-name 'other-remotes))
                         (valid-other-remotes (-filter #'stp-valid-remote-p other-remotes)))
                    (setq pkg-info (stp-set-attribute pkg-info pkg-name 'other-remotes valid-other-remotes)))
                  (cl-case method
                    (git
                     ;; First make sure that the remote is valid. This has to be done
                     ;; first since `stp-git-subtree-version' needs to know the
                     ;; remote.
                     (let ((remote (stp-get-attribute pkg-info pkg-name 'remote)))
                       (unless (stp-git-valid-remote-p remote)
                         (setq remote (funcall callback 'invalid-git-remote pkg-info pkg-name)))
                       (if remote
                           (setq pkg-info (stp-set-attribute pkg-info pkg-name 'remote remote))
                         (unless quiet
                           (message "Failed to determine the remote of %s" pkg-name))))
                     (db (version update)
                         (stp-git-subtree-version pkg-info pkg-name)
                       ;; Use callback to determine the version if it could not be
                       ;; deduced above.
                       (setq version (or version (funcall callback 'unknown-git-version pkg-info pkg-name)))
                       (if version
                           (progn
                             (setq pkg-info (stp-set-attribute pkg-info pkg-name 'version version)))
                         (unless quiet
                           (message "Failed to determine the version of %s" pkg-name)))
                       ;; Use callback to determine update if it could not be deduced
                       ;; above.
                       (setq update (or update (funcall callback 'unknown-git-update pkg-info pkg-name)))
                       (if update
                           (progn
                             (setq pkg-info (stp-set-attribute pkg-info pkg-name 'update update))
                             (when (eq update 'unstable)
                               ;; If the 'update attribute is 'unstable, there
                               ;; should be a 'branch attribute. If it is missing,
                               ;; we try to get it from the callback function. If
                               ;; that doesn't work, we assume that it should be the
                               ;; master branch.
                               (let ((branch (stp-get-attribute pkg-info pkg-name 'branch)))
                                 (setq branch
                                       (or branch
                                           (funcall callback 'unknown-git-branch pkg-info pkg-name)))
                                 (if branch
                                     (progn
                                       (setq pkg-info (stp-set-attribute pkg-info pkg-name 'branch branch)))
                                   (unless quiet
                                     (message "Failed to determine the update mechanism for %s" pkg-name)))))))))
                    (elpa
                     (setq pkg-info (funcall callback 'partial-elpa-package pkg-info pkg-name)))
                    (url
                     (setq pkg-info (funcall callback 'partial-url-package pkg-info pkg-name)))
                    ;; nil means that we were unable to determine the method. In
                    ;; this case, we obtain the information via callbacks.
                    ((nil)
                     (setq pkg-info (funcall callback 'unknown-package pkg-info pkg-name)))))
              (when (funcall callback 'ghost-package pkg-info pkg-name)
                (setq pkg-info (cl-remove-if (lambda (cell) (string= (car cell) pkg-name)) pkg-info))))
            (cl-incf i)))
      ;; Ensure that the package info file is updated even on a keyboard quit or
      ;; other signal.
      (funcall callback 'pkg-info-updated pkg-info nil)))
  pkg-info)

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

(defun stp-commit-push-args ()
  (if current-prefix-arg
      (list :do-commit (not stp-auto-commit)
            :do-push (and (not stp-auto-commit) (not stp-auto-push)))
    (list :do-commit stp-auto-commit :do-push stp-auto-push)))

(defun stp-commit-push-action-args ()
  (append (stp-commit-push-args)
          (list :do-actions (if current-prefix-arg
                                (not stp-auto-post-actions)
                                stp-auto-post-actions))))

(defun stp-list-package-on-line (&optional offset)
  (when (derived-mode-p 'stp-list-mode)
    (setq offset (or offset 0))
    (let ((line-num (line-number-at-pos)))
      (save-excursion
        (forward-line offset)
        (when (= (line-number-at-pos) (+ line-num offset))
          (let ((pkg-name (rem-plain-symbol-at-point)))
            (and (not (string= pkg-name ""))
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
  (pcase-let* ((args (if actions
                         (stp-commit-push-action-args)
                       (stp-commit-push-args)))
               (do-commit (plist-get args :do-commit))
               (proceed (or (not do-commit)
                            (stp-git-clean-or-ask-p)))
               (`(,pkg-name . ,pkg-alist) (and proceed
                                               read-pkg-alist
                                               (stp-read-package :line-pkg line-pkg)))
               (pkg-name (and proceed
                              (or pkg-name
                                  (if line-pkg
                                      (stp-list-read-package "Package name: ")
                                    (stp-read-name "Package name: "))))))
    (append (list pkg-name)
            (when read-pkg-alist
              (list pkg-alist))
            args)))

(cl-defun stp-install (pkg-name pkg-alist &key do-commit do-push do-actions (refresh t))
  "Install a package named pkg-name that has the alist pkg-alist. If
do-commit is non-nil, then automatically commit to the Git
repository after installing the package. If both do-commit and
do-push are non-nil then push to the remote repository as well.
If do-actions is non-nil, `stp-post-actions' will be called after
the package has been installed.

Interactively, do-commit, do-push and do-actions are set
according to `stp-auto-commit', `stp-auto-push', and
`stp-auto-post-actions'. With a prefix argument, each of these is
negated relative to the default."
  (interactive (stp-command-args :actions t :read-pkg-alist t :line-pkg nil))
  ;; pkg-name may be nil in interactive calls when the user answers no when the
  ;; repository is dirty and `stp-git-clean-or-ask-p' is called.
  (when pkg-name
    (let ((pkg-info (stp-read-info)))
      (save-window-excursion
        (let-alist pkg-alist
          (let ((chosen-remote (stp-choose-remote "Remote: " .remote .other-remotes)))
            (when (stp-url-safe-remote-p chosen-remote)
              (cl-ecase .method
                (git (stp-git-install pkg-info pkg-name chosen-remote .version .update :branch .branch))
                (elpa (stp-elpa-install pkg-info pkg-name chosen-remote .version))
                (url (stp-url-install pkg-info pkg-name chosen-remote .version)))
              (stp-update-remotes pkg-info pkg-name chosen-remote .remote .other-remotes)
              (stp-write-info pkg-info)
              (stp-git-commit-push (format "Installed version %s of %s" .version pkg-name) do-commit do-push)
              (when do-actions
                (stp-post-actions pkg-name))
              (when refresh
                (stp-list-refresh pkg-name t)))))))))

(cl-defun stp-uninstall (pkg-name &key do-commit do-push (refresh t))
  "Uninstall the package named pkg-name. The do-commit and do-push arguments are
as in `stp-install'."
  (interactive (stp-command-args))
  (when pkg-name
    (let* ((pkg-info (stp-read-info))
           (version (stp-get-attribute pkg-info pkg-name 'version)))
      (save-window-excursion
        (stp-with-package-source-directory
          (if (eql (call-process-shell-command (format "git rm -r '%s'" pkg-name)) 0)
              (progn
                (delete-directory pkg-name t)
                (setq pkg-info (map-delete pkg-info pkg-name))
                (stp-write-info pkg-info)
                (stp-delete-load-path pkg-name)
                (stp-git-commit-push (format "Uninstalled version %s of %s" version pkg-name) do-commit do-push)
                (when refresh
                  (let ((other-pkg-name (stp-list-other-package)))
                    (stp-list-refresh other-pkg-name t))))
            (error "Failed to remove %s. This can happen when there are uncommitted changes in the git repository" pkg-name)))))))

(defvar stp-git-upgrade-always-offer-remote-heads t)

(cl-defun stp-upgrade (pkg-name &key do-commit do-push do-actions (refresh t))
  "Change the version of the package named pkg-name. The do-commit,
do-push and proceed arguments are as in `stp-install'."
  (interactive (stp-command-args :actions t))
  (when pkg-name
    (let ((pkg-info (stp-read-info)))
      (save-window-excursion
        (let-alist (stp-get-alist pkg-info pkg-name)
          (let* ((chosen-remote (stp-choose-remote "Remote: " .remote .other-remotes))
                 (extra-versions (and (or stp-git-upgrade-always-offer-remote-heads
                                          (eq .update 'unstable))
                                      (stp-git-remote-heads-sorted chosen-remote)))
                 (prompt (format "Upgrade from %s to version: " .version)))
            (when (stp-url-safe-remote-p chosen-remote)
              (when (and .branch (member .branch extra-versions))
                (setq extra-versions (cons .branch (remove .branch extra-versions))))
              (cl-ecase .method
                (git (->> extra-versions
                          (stp-git-read-version prompt chosen-remote :extra-versions-position (if (eq .update 'unstable) 'first 'last) :extra-versions)
                          (stp-git-upgrade pkg-info pkg-name chosen-remote)))
                (elpa (->> (stp-elpa-read-version prompt pkg-name chosen-remote)
                           (stp-elpa-upgrade pkg-info pkg-name chosen-remote)))
                (url (->> (stp-url-read-version prompt)
                          (stp-url-upgrade pkg-info pkg-name chosen-remote))))
              (let ((new-version (stp-get-attribute pkg-info pkg-name 'version)))
                (stp-update-remotes pkg-info pkg-name chosen-remote .remote .other-remotes)
                (stp-write-info pkg-info)
                (stp-git-commit-push (format "Installed version %s of %s" new-version pkg-name) do-commit do-push)
                (when do-actions
                  (stp-post-actions pkg-name))
                (when refresh
                  (stp-list-refresh pkg-name t))))))))))

(cl-defun stp-repair (pkg-name &key do-commit do-push (refresh t))
  "Repair the package named pkg-name. The do-commit, do-push and proceed
arguments are as in `stp-install'."
  (interactive (stp-command-args))
  (when pkg-name
    (let ((pkg-info (stp-read-info)))
      (save-window-excursion
        (stp-with-package-source-directory
          (stp-write-info (stp-repair-info pkg-info :quiet nil :pkg-names (list pkg-name)))
          (stp-git-commit-push (format "Repaired the source package %s" pkg-name) do-commit do-push)
          (when refresh
            (stp-list-refresh pkg-name t)))))))

(cl-defun stp-repair-all (&key do-commit do-push (refresh t) interactive-p)
  "Run `stp-repair-info' and write the repaired package info to
`stp-info-file'"
  (interactive (append (stp-commit-push-args) (list :interactive-p t)))
  (when (and interactive-p
             (stp-git-clean-or-ask-p))
    (let ((refresh-pkg-name (stp-list-package-on-line)))
      (save-window-excursion
        (stp-with-package-source-directory
          (stp-write-info (stp-repair-info (stp-read-info) :quiet nil))
          (stp-git-commit-push (format "Repaired source packages") do-commit do-push)
          (when refresh
            (stp-list-refresh refresh-pkg-name t)))))))

(cl-defun stp-toggle-update (pkg-name &key do-commit do-push (refresh t))
  "Toggle the update attribute for the package named pkg-name between stable and
unstable."
  (interactive (stp-command-args))
  (when pkg-name
    (let* ((pkg-info (stp-read-info))
           (method (stp-get-attribute pkg-info pkg-name 'method)))
      (if (eq method 'git)
          (let ((update (stp-get-attribute pkg-info pkg-name 'update)))
            (stp-set-attribute pkg-info pkg-name 'update (stp-invert-update update))
            (if (eq update 'stable)
                (let ((branch (or (stp-get-attribute pkg-info pkg-name 'branch)
                                  (stp-git-read-branch "Branch: "
                                                       (stp-get-attribute pkg-info
                                                                          pkg-name
                                                                          'remote)))))
                  (stp-set-attribute pkg-info pkg-name 'branch branch))
              (stp-remove-attribute pkg-info pkg-name 'branch))
            (stp-write-info pkg-info)
            (stp-git-commit-push (format "Changed update to %s for %s"
                                         (stp-invert-update update)
                                         pkg-name)
                                 do-commit
                                 do-push)
            (when refresh
              (stp-list-refresh pkg-name t)))
        (user-error "The update attribute can only be toggled for git packages.")))))

(defun stp-post-actions (pkg-name)
  "Perform actions that are necessary after a package is installed or upgraded
such as building, updating info directories and updating the load path."
  (interactive (list (stp-list-read-package "Package name: ")))
  (stp-update-load-path (stp-absolute-path pkg-name))
  (stp-reload pkg-name)
  (stp-build pkg-name)
  (stp-build-info pkg-name)
  (stp-update-info-directories pkg-name))

(defvar stp-build-output-buffer-name "*STP Build Output*")

(defvar stp-allow-naive-byte-compile nil
  "If non-nil, packages without a Makefile will be byte-compiled
naively. This might cause problems if the packages need to be
byte-compiled in some special way.")

(defun stp-build (pkg-name &optional allow-naive-byte-compile)
  "If needed, build the package PKG-NAME by running the appropriate
build systems or performing byte compilation. When
ALLOW-NAIVE-BYTE-COMPILE is non-nil, byte compilation will be
performed even when no build system is present. Interactively, Return non-nil if
there were no errors."
  (interactive (list (stp-list-read-package "Package name: ")
                     (xor stp-allow-naive-byte-compile current-prefix-arg)))
  (save-window-excursion
    (stp-with-package-source-directory
      (let* ((output-buffer stp-build-output-buffer-name)
             (pkg-path (stp-absolute-path pkg-name))
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
          (rem-with-directory build-dir
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
                   ;; Handle GNU make. We use a separate `rem-with-directory' here
                   ;; because the cmake code above can change build-dir.
                   (rem-with-directory build-dir
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
                        (rem-with-directory pkg-path
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
          success)))))

(defvar stp-build-blacklist nil
  "This is a list of packages that should not be built by
`stp-build-all' when it is called interactively.")

(defun stp-build-all (&optional pkg-names allow-naive-byte-compile)
  "Build the packages that need it."
  (interactive (list (cl-set-difference (stp-filesystem-names)
                                        stp-build-blacklist
                                        :test #'equal)
                     (xor stp-allow-naive-byte-compile current-prefix-arg)))
  (stp-with-package-source-directory
    (let (failed)
      (dolist (pkg-name pkg-names)
        (message "Building %s" pkg-name)
        (when (not (stp-build pkg-name))
          (push pkg-name failed)))
      (if failed
          (message "Failed to build: %s" (s-join " " failed))
        (message "Successfully built all packages")))))

(defun stp-build-info (pkg-name)
  "Build the info manuals for PKG-NAME."
  (interactive (list (stp-list-read-package "Package name: ")))
  (let* ((makefiles (f-entries (stp-absolute-path pkg-name)
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
                  (rem-with-directory (f-dirname makefile)
                    (setq attempted t)
                    (message "Makefile with target %s found in %s. Attempting to run make..." target (f-dirname makefile))
                    (let ((cmd (format "make %s" target)))
                      (stp-before-build-command cmd output-buffer)
                      (if (= (call-process-shell-command cmd nil output-buffer) 0)
                          (progn
                            (message "Built the info manual for %s using make" pkg-name)
                            (cl-return t))
                        (message "'make %s' failed in %s" target (f-dirname makefile)))))))

              ;; Try to compile a texi file directly.
              (dolist (source (f-entries (stp-absolute-path pkg-name)
                                         (lambda (path)
                                           (string= (f-filename path) texi-target))
                                         t))
                (rem-with-directory (f-dirname source)
                  (setq attempted t)
                  (message "texi source file found at %s. Attempting to compile it with makeinfo..." source)
                  (cond
                   (;; Don't build texi files unless they have changed since the info
                    ;; manual was last built.
                    (f-newer-p (f-swap-ext source "info") source)
                    (message "The info manual for %s is up to date" pkg-name)
                    (cl-return t))
                   ((let ((cmd (format "makeinfo --no-split %s" texi-target)))
                      (progn
                        (stp-before-build-command cmd output-buffer)
                        (= (call-process-shell-command cmd nil output-buffer) 0)))
                    (message "Built the info manual for %s using makeinfo" pkg-name)
                    (cl-return t))
                   (t
                    (message "makeinfo failed" source))))))))
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
      (when (not (stp-build-info pkg-name))
        (push pkg-name failed)))
    (if failed
        (message "Failed to build info manuals for: %s" (s-join " " failed))
      (message "Successfully built info manuals for all packages"))))

(defun stp-reload (pkg-name)
  "Reload the package."
  (interactive (list (stp-list-read-package "Package name: ")))
  (let* ((pkg-path (stp-absolute-path pkg-name))
         ;; Reload those features that were already loaded and correspond to
         ;; files in the package.
         (reload-features (->> (directory-files-recursively pkg-path
                                                            (concat "\\.\\("
                                                                    rem-elisp-file-regexp
                                                                    "\\)$"))
                               (mapcar (lambda (path)
                                         (intern (car (last (f-split (f-base path)))))))
                               cl-remove-duplicates
                               (cl-intersection features))))
    (setq features (cl-set-difference features reload-features))
    (dolist (f reload-features)
      (require f))
    (message "Reloaded %s" pkg-name)))

(defun stp-update-info-directories (pkg-name &optional quiet)
  "By default, detect info files for all source packages in
`stp-source-directory' and add their directories to
`Info-directory-list'. If PKG-NAME is non-nil, only search for
info files in the directory for that package."
  (interactive (list (stp-list-read-package "Package name: ")))
  (let* ((directory (stp-absolute-path pkg-name))
         (new (mapcar 'f-dirname
                      (f-entries directory
                                 (-partial #'string-match-p "\\.info$")
                                 t))))
    (info-initialize)
    (setq Info-directory-list
          (cl-remove-duplicates (append Info-directory-list new)
                                :test 'equal))
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
  '((t (:inherit font-lock-warning-face)))
  "Face for packages with errors")

(defvar stp-list-font-lock-keywords '((("^.*\\?.*" . stp-list-error-face))))

(defvar stp-list-buffer-name "*STP Package List*")

(defvar stp-list-missing-field-string "???")

(define-derived-mode stp-list-mode fundamental-mode "STPPackage"
  "Major mode for managing source packages. \\{stp-list-mode-map}"
  (setq-local font-lock-defaults stp-list-font-lock-keywords)
  ;; Without this, fontification will not be done on parts of the buffer that
  ;; are not visible which will break `stp-list-next-repair'.
  (font-lock-ensure))

(defun stp-list-next-package (&optional n)
  "Go to the next package. With a prefix argument, go forward that many packages.
With a negative prefix argument, go backward that many packages."
  (interactive "p")
  (forward-line n))

(defun stp-list-goto-package (&optional arg)
  "In `stp-list-mode', open the source file that shares the same
name as the package on the current line if such a file exists.
With a prefix argument, open the directory for the current
package."
  (interactive "P")
  (let* ((pkg-name (stp-list-read-package "Package name: "))
         (pkg-file (concat pkg-name ".el"))
         (pkg-path (stp-absolute-path pkg-name))
         (paths (-sort (lambda (path path2)
                         (or (< (rem-path-length path)
                                (rem-path-length path2))
                             (and (= (rem-path-length path)
                                     (rem-path-length path2))
                                  (string< path path2))))
                       (directory-files-recursively pkg-path (regexp-quote pkg-file))))
         (path (car paths)))
    (if arg
        (dired pkg-path)
      (if path
          (find-file path)
        (message "No file named %s found" pkg-file)))))

(defun stp-list-previous-package (&optional n)
  "Go to the previous package. With a prefix argument, go backward
that many packages. With a negative prefix argument, go forward
that many packages."
  (interactive "p")
  (setq n (or n 1))
  (forward-line (- n)))

(defun stp-list-next-repair (&optional n)
  "Go to the next package that needs to be repaired. With a prefix argument, go
forward that many packages. With a negative prefix argument, go backward that
many packages."
  (interactive "p")
  (setq n (or n 1))
  (db (line-move-fun search-fun)
      (if (>= n 0)
          (list #'end-of-line #'text-property-search-forward)
        (list #'beginning-of-line #'text-property-search-backward))
    (setq n (abs n))
    (while (and (> n 0)
                (prog2
                    (funcall line-move-fun)
                    (funcall search-fun 'face 'stp-list-error-face)
                  (beginning-of-line)))
      (cl-decf n))))

(defun stp-list-previous-repair (&optional n)
  "Go to the previous package that needs to be repaired. With a prefix argument,
go backward that many packages. With a negative prefix argument, go forward
that many packages."
  (interactive "p")
  (setq n (or n 1))
  (stp-list-next-repair (- n)))

(rem-set-keys stp-list-mode-map
              "b" #'stp-build
              "B" #'stp-build-all
              "m" #'stp-build-info
              "M" #'stp-build-all-info
              "d" #'stp-uninstall
              "g" #'stp-list-refresh
              "G" #'stp-reload
              "i" #'stp-install
              "I" #'stp-update-all-info-directories
              "k" #'stp-uninstall
              "l" #'stp-update-load-path
              "L" #'stp-update-load-paths
              "n" #'stp-list-next-package
              "p" #'stp-list-previous-package
              "M-n" #'stp-list-next-repair
              "M-p" #'stp-list-previous-repair
              "r" #'stp-repair
              "R" #'stp-repair-all
              "t" #'stp-toggle-update
              "u" #'stp-upgrade
              "RET" #'stp-list-goto-package)

(cl-defun stp-list-refresh (&optional refresh-pkg-name quiet)
  (interactive (list (stp-list-package-on-line)))
  (when (derived-mode-p 'stp-list-mode)
    (let ((pkg-info (stp-read-info))
          (column (current-column))
          (window-line-num (progn
                             (beginning-of-line)
                             (rem-window-line-number-at-pos))))
      (read-only-mode 0)
      (erase-buffer)
      (insert "Package Version Method Update Branch Remote\n")
      (dolist (pkg-name (stp-info-names))
        (let-alist (stp-get-alist pkg-info pkg-name)
          (insert (format "%s %s %s %s %s %s\n"
                          (stp-name pkg-name)
                          (or .version stp-list-missing-field-string)
                          (if .method
                              (symbol-name .method)
                            stp-list-missing-field-string)
                          ;; Instead of using
                          ;; `stp-list-missing-field-string' when the
                          ;; update or branch is missing, simply omit it.
                          (or .update "\t")
                          (or .branch "\t")
                          (or .remote stp-list-missing-field-string)))))
      ;; Align columns. We explicitly use a space so that tab characters will
      ;; count as a column (see how branches are handled above).
      (let ((align-large-region nil))
        (align-regexp (point-min) (point-max) "\\( *\\) +" nil nil t))
      (goto-char (point-min))
      (read-only-mode 1)
      (when refresh-pkg-name
        (re-search-forward (concat "^" refresh-pkg-name " "))
        (rem-move-current-window-line-to-pos window-line-num)
        (beginning-of-line)
        (forward-char column))
      (unless quiet
        (message "Refreshed packages")))))

(defun stp-list-focus-package (pkg-name)
  (save-match-data
    (re-search-forward (concat "^" (regexp-quote pkg-name)))
    (beginning-of-line)
    (recenter)))

(defun stp-list ()
  "List the packages installed in `stp-source-directory'."
  (interactive)
  (let* ((default-directory stp-source-directory)
         (buf (get-buffer-create stp-list-buffer-name)))
    (pop-to-buffer buf)
    (stp-list-mode)
    (stp-list-refresh nil t)))

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
  (let ((deleted-dirs 0)
        (deleted-entries 0)
        (filesystem-pkgs (stp-filesystem-names))
        (info-pkgs (stp-info-names)))
    (when (memq orphan-type '(info both))
      (let ((k 0)
            (pkg-info (stp-read-info))
            (orphaned-info-names (cl-set-difference info-pkgs filesystem-pkgs :test #'equal)))
        (unwind-protect
            (dolist (target-name orphaned-info-names)
              (setq pkg-info (cl-delete-if (lambda (pkg)
                                             (let ((name (car pkg)))
                                               (and (string= name target-name)
                                                    (cl-incf k)
                                                    (or (not confirm)
                                                        (yes-or-no-p (format "(%d/%d) The directory for %s in %s is missing. Remove the entry in %s?" k (length orphaned-info-names) name stp-source-directory stp-info-file)))
                                                    (cl-incf deleted-entries))))
                                           pkg-info)))
          ;; Make sure that changes are written to disk each time so that
          ;; progress isn't lost of the user aborts.
          (stp-write-info pkg-info))))
    (when (memq orphan-type '(filesystem both))
      (let ((k 1)
            (orphaned-dir-names (cl-set-difference filesystem-pkgs info-pkgs :test #'equal)))
        (dolist (dir orphaned-dir-names)
          (when (or (not confirm)
                    (yes-or-no-p (format "(%d/%d) The directory %s in %s has no entry in %s. Delete the directory?" k (length orphaned-dir-names) dir stp-source-directory stp-info-file)))
            (delete-directory (stp-absolute-path dir) t)
            (cl-incf deleted-dirs))
          (cl-incf k))))
    (message "Deleted %d orphaned entries in %s and %d orphaned directories in %s"
             deleted-entries
             stp-info-file
             deleted-dirs
             stp-source-directory)))

(provide 'stp)
;;; subtree-package.el ends here
